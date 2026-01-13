# Automerge sync client for fetching documents from remote servers

# Helper to format raw bytes as hex preview
format_hex <- function(bytes, max_len = 20L) {
  paste(
    sprintf("%02x", as.integer(bytes[1:min(max_len, length(bytes))])),
    collapse = " "
  )
}

#' Fetch a document from a sync server
#'
#' Connects to an automerge-repo sync server and retrieves a document by ID.
#' This is useful for debugging sync issues or fetching documents from remote
#' servers like sync.automerge.org.
#'
#' @param url WebSocket URL of the sync server (e.g., "ws://localhost:3030/"
#'   or "wss://sync.automerge.org/"). Note: trailing slash may be required.
#' @param doc_id Document ID (base58check encoded string)
#' @param timeout Timeout in milliseconds for each receive operation. Default 5000.
#' @param tls (optional) for secure wss:// connections to servers with
#'   self-signed or custom CA certificates, a TLS configuration object
#'   created by [nanonext::tls_config()].
#' @param verbose Logical, print debug messages. Default FALSE.
#'
#' @return An automerge document object containing the fetched data.
#'
#' @details
#' The function implements the automerge-repo sync protocol:
#' 1. Connects via WebSocket
#' 2. Sends join message with peer ID
#' 3. Receives peer response from server
#' 4. Sends sync request for the specified document
#' 5. Receives and applies sync messages until complete
#'
#' Sync is considered complete when no new messages arrive within the timeout
#' after at least one sync round.
#'
#' @examplesIf interactive()
#' # Fetch from public sync server
#' doc <- amsync_fetch("wss://sync.automerge.org", "4F63WJPDzbHkkfKa66h1Qrr1sC5U")
#'
#' # Fetch from local server with debug output
#' doc <- amsync_fetch("ws://localhost:3030", "myDocId", verbose = TRUE)
#'
#' # Fetch from server with self-signed certificate
#' cert <- nanonext::write_cert()
#' tls <- nanonext::tls_config(client = cert$client)
#' doc <- amsync_fetch("wss://localhost:3030", "myDocId", tls = tls)
#'
#' # Inspect the document
#' automerge::am_keys(doc)
#'
#' @export
amsync_fetch <- function(
  url,
  doc_id,
  timeout = 5000L,
  tls = NULL,
  verbose = FALSE
) {
  doc <- am_create()
  sync_state <- am_sync_state_new()

  peer_id <- generate_peer_id()

  if (verbose) {
    message("[CLIENT] Connecting to ", url)
  }
  if (verbose) {
    message("[CLIENT] Our peer ID: ", peer_id)
  }
  if (verbose) {
    message("[CLIENT] Requesting document: ", doc_id)
  }

  s <- stream(
    dial = url,
    tls = tls,
    textframes = FALSE
  )
  on.exit(close(s))

  if (verbose) {
    message("[CLIENT] Connected, sending join message")
  }

  join <- list(
    type = "join",
    senderId = peer_id,
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("1")
  )
  join_bytes <- cborenc(join)
  if (verbose) {
    message(
      "[CLIENT] Sending join (",
      length(join_bytes),
      " bytes): ",
      format_hex(join_bytes)
    )
  }
  send(s, join_bytes, mode = "raw", block = TRUE)

  if (verbose) {
    message("[CLIENT] Waiting for peer response...")
  }
  peer_raw <- recv(s, mode = "raw", block = timeout)

  if (inherits(peer_raw, "errorValue")) {
    stop("Failed to receive peer response: ", peer_raw)
  }

  if (verbose) {
    message(
      "[CLIENT] Received ",
      length(peer_raw),
      " bytes: ",
      format_hex(peer_raw)
    )
  }

  peer_msg <- cbordec(peer_raw)

  if (verbose) {
    message("[CLIENT] Message type: ", peer_msg$type)
  }

  if (peer_msg$type == "error") {
    stop("Server error: ", peer_msg$message)
  }

  if (peer_msg$type != "peer") {
    stop("Expected peer message, got: ", peer_msg$type)
  }

  server_peer_id <- peer_msg$senderId
  if (verbose) {
    message("[CLIENT] Server peer ID: ", server_peer_id)
    message("[CLIENT] Protocol version: ", peer_msg$selectedProtocolVersion)
  }

  sync_data <- am_sync_encode(doc, sync_state)
  if (verbose) {
    message(
      "[CLIENT] Initial sync data: ",
      if (is.null(sync_data)) "NULL" else paste(length(sync_data), "bytes")
    )
  }

  request <- list(
    type = "request",
    senderId = peer_id,
    targetId = server_peer_id,
    documentId = doc_id,
    data = sync_data %||% raw(0)
  )
  request_bytes <- cborenc(request)
  if (verbose) {
    message("[CLIENT] Sending request (", length(request_bytes), " bytes)")
  }
  send(s, request_bytes, mode = "raw", block = TRUE)

  # Sync loop - receive and process messages until timeout
  sync_rounds <- 0L
  idle_timeout <- 2000L # Wait 2 seconds for additional messages after sync

  repeat {
    # Receive with timeout
    result <- recv(s, mode = "raw", block = idle_timeout)

    if (inherits(result, "errorValue")) {
      # Timeout or error - no more messages
      if (verbose) {
        message("[CLIENT] No more messages (timeout)")
      }
      break
    }

    if (verbose) {
      message(
        "[CLIENT] Received ",
        length(result),
        " bytes: ",
        format_hex(result)
      )
    }

    msg <- tryCatch(
      cbordec(result),
      error = function(e) {
        if (verbose) {
          message("[CLIENT] CBOR decode error: ", conditionMessage(e))
        }
        return(NULL)
      }
    )

    if (is.null(msg)) {
      next
    }

    if (verbose) {
      message("[CLIENT] Message type: ", msg$type)
    }

    if (msg$type == "sync") {
      sync_rounds <- sync_rounds + 1L
      data_len <- if (is.null(msg$data)) 0L else length(msg$data)
      if (verbose) {
        message(
          "[CLIENT] Sync round ",
          sync_rounds,
          " - received ",
          data_len,
          " bytes of sync data"
        )
      }

      if (length(msg$data)) {
        if (verbose) {
          message("[CLIENT] Sync data hex: ", format_hex(msg$data, 30L))
        }
        tryCatch(
          {
            am_sync_decode(doc, sync_state, msg$data)
            if (verbose) message("[CLIENT] Applied sync data successfully")
          },
          error = function(e) {
            warning("[CLIENT] am_sync_decode error: ", conditionMessage(e))
          }
        )
      }

      if (verbose) {
        keys <- tryCatch(am_keys(doc), error = function(e) character(0))
        message(
          "[CLIENT] Document now has ",
          length(keys),
          " keys: ",
          paste(keys[1:min(5, length(keys))], collapse = ", "),
          if (length(keys) > 5) "..." else ""
        )
      }

      reply_data <- am_sync_encode(doc, sync_state)
      if (!is.null(reply_data)) {
        if (verbose) {
          message("[CLIENT] Sending ", length(reply_data), " bytes in response")
        }
        response <- list(
          type = "sync",
          senderId = peer_id,
          targetId = server_peer_id,
          documentId = doc_id,
          data = reply_data
        )
        send(s, cborenc(response), mode = "raw", block = TRUE)
      } else {
        if (verbose) message("[CLIENT] No more data to send from our side")
      }
    } else if (msg$type == "error") {
      stop("Server error: ", msg$message)
    } else if (msg$type == "doc-unavailable") {
      stop("Document not available on server: ", doc_id)
    } else {
      if (verbose) message("[CLIENT] Ignoring message type: ", msg$type)
    }
  }

  if (sync_rounds == 0L) {
    warning("No sync messages received - document may be empty or unavailable")
  }

  if (verbose) {
    keys <- tryCatch(am_keys(doc), error = function(e) character(0))
    message("[CLIENT] Fetch complete after ", sync_rounds, " sync rounds")
    message("[CLIENT] Document has ", length(keys), " top-level keys")
    if (length(keys) > 0L) {
      message("[CLIENT] Keys: ", paste(keys, collapse = ", "))
    }
  }

  doc
}


#' Inspect a document from a sync server
#'
#' Fetches a document and prints its structure for debugging.
#'
#' @param url WebSocket URL of the sync server.
#' @param doc_id Document ID (base58check encoded string).
#' @param timeout Timeout in milliseconds. Default 5000.
#' @param tls (optional) for secure wss:// connections to servers with
#'   self-signed or custom CA certificates, a TLS configuration object
#'   created by [nanonext::tls_config()].
#' @param max_depth Maximum depth to recurse into nested structures. Default 2.
#'
#' @return Invisibly returns the fetched automerge document.
#'
#' @examples
#' \dontrun{
#' # Inspect document structure from public server
#' amsync_inspect("wss://sync.automerge.org/", "4F63WJPDzbHkkfKa66h1Qrr1sC5U")
#' }
#'
#' @export
amsync_inspect <- function(
  url,
  doc_id,
  timeout = 5000L,
  tls = NULL,
  max_depth = 2
) {
  cat("Fetching document:", doc_id, "\n")
  cat("From server:", url, "\n\n")

  doc <- amsync_fetch(
    url,
    doc_id,
    timeout = timeout,
    tls = tls,
    verbose = TRUE
  )

  cat("\n=== Document Structure ===\n")
  print_doc_structure(doc, max_depth = max_depth)

  invisible(doc)
}


#' Print document structure recursively
#'
#' @param doc Automerge document or sub-object
#' @param prefix Indentation prefix
#' @param max_depth Maximum recursion depth
#' @param current_depth Current recursion depth
#'
#' @noRd
print_doc_structure <- function(
  doc,
  obj = AM_ROOT,
  prefix = "",
  max_depth = 2,
  current_depth = 0
) {
  keys <- tryCatch(am_keys(doc, obj), error = function(e) character(0))

  if (length(keys) == 0L) {
    cat(prefix, "(empty)\n", sep = "")
    return(invisible())
  }

  for (key in keys) {
    val <- tryCatch(am_get(doc, obj, key), error = function(e) NULL)

    if (is.null(val)) {
      cat(prefix, key, ": NULL\n", sep = "")
    } else if (is.character(val) && length(val) == 1L) {
      display <- if (nchar(val) > 60) paste0(substr(val, 1, 57), "...") else val
      cat(prefix, key, ': "', display, '"\n', sep = "")
    } else if (is.numeric(val) && length(val) == 1L) {
      cat(prefix, key, ": ", val, "\n", sep = "")
    } else if (is.logical(val) && length(val) == 1L) {
      cat(prefix, key, ": ", if (val) "true" else "false", "\n", sep = "")
    } else if (inherits(val, "am_list")) {
      len <- tryCatch(am_length(doc, val), error = function(e) NA)
      cat(prefix, key, ": [list, length ", len, "]\n", sep = "")
      if (current_depth < max_depth && !is.na(len) && len > 0L) {
        for (i in seq_len(min(len, 5L))) {
          item <- tryCatch(am_get(doc, val, i), error = function(e) NULL)
          cat(prefix, "  [", i, "]: ", sep = "")
          if (is.character(item)) {
            cat(
              '"',
              substr(item, 1, 40),
              '"',
              if (nchar(item) > 40) "..." else "",
              "\n",
              sep = ""
            )
          } else if (inherits(item, "am_obj_id")) {
            cat("{object}\n")
            print_doc_structure(
              doc,
              item,
              paste0(prefix, "    "),
              max_depth,
              current_depth + 1
            )
          } else {
            cat(class(item)[1], "\n")
          }
        }
        if (len > 5L) {
          cat(prefix, "  ... and ", len - 5L, " more items\n", sep = "")
        }
      }
    } else if (inherits(val, "am_obj_id")) {
      cat(prefix, key, ": {object}\n", sep = "")
      if (current_depth < max_depth) {
        print_doc_structure(
          doc,
          val,
          paste0(prefix, "  "),
          max_depth,
          current_depth + 1
        )
      }
    } else {
      cat(prefix, key, ": <", class(val)[1], ">\n", sep = "")
    }
  }

  invisible()
}
