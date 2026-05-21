# Automerge sync client for fetching documents from remote servers

# Helper to format raw bytes as hex preview
format_hex <- function(bytes, max_len = 20L) {
  paste(
    sprintf("%02x", as.integer(bytes[1:min(max_len, length(bytes))])),
    collapse = " "
  )
}

send_msg <- function(s, msg) {
  send(s, cborenc(msg), mode = "raw", block = TRUE)
}

recv_msg <- function(s, timeout = NULL) {
  aio <- recv_aio(s, mode = "raw", timeout = timeout)
  while (unresolved(aio)) {
    run_now(1L)
  }
  aio$data
}

sync_msg <- function(type, peer_id, target_id, doc_id, data) {
  list(
    type = type,
    senderId = peer_id,
    targetId = target_id,
    documentId = doc_id,
    data = data
  )
}

apply_sync_and_reply <- function(
  s,
  doc,
  sync_state,
  data,
  peer_id,
  target_id,
  doc_id
) {
  if (length(data)) {
    tryCatch(
      am_sync_decode(doc, sync_state, data),
      error = function(e) {
        warning("am_sync_decode error: ", conditionMessage(e))
      }
    )
  }
  reply <- am_sync_encode(doc, sync_state)
  if (!is.null(reply)) {
    send_msg(s, sync_msg("sync", peer_id, target_id, doc_id, reply))
  }
  invisible()
}

bearer_headers <- function(token) {
  if (!is.null(token)) c(Authorization = paste("Bearer", token)) else NULL
}

join_msg <- function(peer_id) {
  list(
    type = "join",
    senderId = peer_id,
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("1")
  )
}

#' Create a persistent sync client
#'
#' Connects to an automerge-repo sync server and maintains a persistent
#' WebSocket connection for continuous document synchronization. Unlike
#' [amsync_fetch()], which performs a one-off retrieval, this client stays
#' connected and receives real-time updates from other peers.
#'
#' @inheritParams amsync_fetch
#' @param sync Sync interval in seconds for pushing local changes to the
#'   server. Default 1. Uses [later::later()] to periodically check for and
#'   send local changes. This is a cheap no-op when there are no changes.
#'
#' @return An environment of class `"amsync_client"` with reference semantics,
#'   containing:
#'   \describe{
#'     \item{`doc`}{The live automerge document, kept in sync with the server.}
#'     \item{`push()`}{Push local changes to the server immediately.}
#'     \item{`close()`}{Disconnect and stop syncing.}
#'     \item{`active`}{Logical, whether the connection is active.}
#'   }
#'
#' @details
#' The client performs a synchronous handshake and initial sync before
#' returning, so `$doc` has meaningful content immediately. After that,
#' incoming changes are received asynchronously via a self-chaining promise
#' loop, and local changes are flushed periodically via a [later::later()]
#' timer.
#'
#' @examplesIf interactive()
#' server <- amsync_server()
#' server$start()
#' doc_id <- create_document(server)
#'
#' client <- amsync_client(server$url, doc_id)
#' automerge::am_keys(client$doc)
#'
#' # Make local changes and push
#' automerge::am_put(client$doc, automerge::AM_ROOT, "key", "value")
#' client$push()
#'
#' # Disconnect
#' client$close()
#' server$close()
#'
#' @export
amsync_client <- function(
  url,
  doc_id,
  timeout = 5000L,
  tls = NULL,
  token = NULL,
  verbose = FALSE,
  sync = 1
) {
  doc <- am_create()
  sync_state <- am_sync_state()
  peer_id <- generate_peer_id()

  if (verbose) {
    message("[CLIENT] Connecting to ", url)
  }

  s <- stream(
    dial = url,
    tls = tls,
    textframes = FALSE,
    headers = bearer_headers(token)
  )

  # --- Synchronous handshake ---

  send_msg(s, join_msg(peer_id))

  if (verbose) {
    message("[CLIENT] Waiting for peer response...")
  }
  peer_raw <- recv_msg(s, timeout = timeout)

  if (inherits(peer_raw, "errorValue")) {
    close(s)
    stop("Failed to receive peer response: ", peer_raw)
  }

  peer_msg <- cbordec(peer_raw)
  if (peer_msg$type == "error") {
    close(s)
    stop("Server error: ", peer_msg$message)
  }
  if (peer_msg$type != "peer") {
    close(s)
    stop("Expected peer message, got: ", peer_msg$type)
  }

  server_peer_id <- peer_msg$senderId
  if (verbose) {
    message("[CLIENT] Server peer ID: ", server_peer_id)
  }

  # --- Send initial request ---

  sync_data <- am_sync_encode(doc, sync_state)
  send_msg(
    s,
    sync_msg("request", peer_id, server_peer_id, doc_id, sync_data %||% raw(0))
  )

  # --- Blocking initial sync ---

  result <- recv_msg(s, timeout = timeout)

  if (!inherits(result, "errorValue")) {
    msg <- cbordec(result)
    if (msg$type == "sync" && length(msg$data)) {
      apply_sync_and_reply(
        s,
        doc,
        sync_state,
        msg$data,
        peer_id,
        server_peer_id,
        doc_id
      )
    } else if (msg$type == "error") {
      close(s)
      stop("Server error: ", msg$message)
    } else if (msg$type == "doc-unavailable") {
      close(s)
      stop("Document not available on server: ", doc_id)
    }
    if (verbose) message("[CLIENT] Initial sync complete")
  }

  # --- Build client environment ---

  client <- new.env(parent = emptyenv())
  client$doc <- doc
  client$active <- TRUE
  client$stream <- s
  client$peer_id <- peer_id
  client$server_peer_id <- server_peer_id
  client$doc_id <- doc_id
  client$sync_state <- sync_state
  client$sync_timer <- NULL
  client$verbose <- verbose

  # Helper: send a sync message for local changes
  send_sync <- function() {
    sync_data <- am_sync_encode(client$doc, client$sync_state)
    if (!is.null(sync_data)) {
      send_msg(
        client$stream,
        sync_msg(
          "sync",
          client$peer_id,
          client$server_peer_id,
          client$doc_id,
          sync_data
        )
      )
      if (client$verbose) message("[CLIENT] Sent sync data")
    }
  }

  # Helper: process an incoming message
  process_message <- function(raw_data) {
    msg <- tryCatch(cbordec(raw_data), error = function(e) NULL)
    if (is.null(msg)) {
      return()
    }

    if (
      msg$type == "sync" &&
        !is.null(msg$documentId) &&
        msg$documentId == client$doc_id
    ) {
      apply_sync_and_reply(
        client$stream,
        client$doc,
        client$sync_state,
        msg$data,
        client$peer_id,
        client$server_peer_id,
        client$doc_id
      )
    } else if (msg$type == "error") {
      warning("Server error: ", msg$message)
    } else if (msg$type == "doc-unavailable") {
      warning("Document not available on server: ", client$doc_id)
    }
  }

  # --- Async receive loop (self-chaining promises) ---

  recv_loop <- function() {
    if (!client$active) {
      return()
    }
    aio <- recv_aio(client$stream, mode = "raw")
    promises::then(
      aio,
      onFulfilled = function(value) {
        process_message(value)
        recv_loop()
      },
      onRejected = function(err) {
        client$active <- FALSE
      }
    )
    invisible()
  }

  # --- Periodic sync loop (outgoing local changes) ---

  sync_loop <- function() {
    if (!client$active) {
      return()
    }
    client$sync_timer <- later(
      function() {
        if (!client$active) {
          return()
        }
        tryCatch(send_sync(), error = function(e) {
          client$active <- FALSE
        })
        sync_loop()
      },
      delay = sync
    )
  }

  # --- Methods ---

  client$push <- send_sync

  client$close <- function() {
    if (!client$active) {
      return(invisible())
    }
    client$active <- FALSE
    if (!is.null(client$sync_timer)) {
      client$sync_timer()
      client$sync_timer <- NULL
    }
    close(client$stream)
    if (client$verbose) {
      message("[CLIENT] Connection closed")
    }
    invisible()
  }

  class(client) <- "amsync_client"

  # Start the async loops
  recv_loop()
  sync_loop()

  client
}

#' @export
print.amsync_client <- function(x, ...) {
  cat("Automerge Sync Client\n")
  cat("  Document:", x$doc_id, "\n")
  cat("  Active:", x$active, "\n")
  invisible(x)
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
#' @param token (optional) JWT (ID token) for authenticated servers.
#'   Sent as a Bearer token in the Authorization header of the WebSocket
#'   upgrade request.
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
#' doc <- amsync_fetch(server$url, "myDocId", verbose = TRUE)
#'
#' # Fetch from server with self-signed certificate
#' cert <- nanonext::write_cert()
#' tls <- nanonext::tls_config(client = cert$client)
#' doc <- amsync_fetch(server$url, "myDocId", tls = tls)
#'
#' # Fetch from authenticated server
#' doc <- amsync_fetch(
#'   "wss://secure.example.com",
#'   "myDocId",
#'   token = "eyJhbGciOi..."
#' )
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
  token = NULL,
  verbose = FALSE
) {
  doc <- am_create()
  sync_state <- am_sync_state()

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
    textframes = FALSE,
    headers = bearer_headers(token)
  )
  on.exit(close(s))

  if (verbose) {
    message("[CLIENT] Connected, sending join message")
  }

  join_bytes <- cborenc(join_msg(peer_id))
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
  peer_raw <- recv_msg(s, timeout = timeout)

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

  request_bytes <- cborenc(
    sync_msg("request", peer_id, server_peer_id, doc_id, sync_data %||% raw(0))
  )
  if (verbose) {
    message("[CLIENT] Sending request (", length(request_bytes), " bytes)")
  }
  send(s, request_bytes, mode = "raw", block = TRUE)

  # Sync loop - receive and process messages until timeout
  sync_rounds <- 0L
  idle_timeout <- 2000L # Wait 2 seconds for additional messages after sync

  repeat {
    result <- recv_msg(s, timeout = idle_timeout)

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

    if (
      msg$type == "sync" && !is.null(msg$documentId) && msg$documentId != doc_id
    ) {
      if (verbose) {
        message(
          "[CLIENT] Ignoring sync for different document: ",
          msg$documentId
        )
      }
      next
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
        send_msg(
          s,
          sync_msg("sync", peer_id, server_peer_id, doc_id, reply_data)
        )
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
    message("[CLIENT] Fetch complete after ", sync_rounds, " sync rounds")
    str(doc)
  }

  doc
}
