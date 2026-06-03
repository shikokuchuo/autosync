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

#' Open a persistent sync connection
#'
#' Connects to an automerge-repo sync server and maintains a persistent
#' WebSocket connection. The connection performs the protocol handshake but
#' holds no documents on its own; open one or more live documents over it with
#' the `$open_doc()` method. Each document stays synced — receiving real-time
#' updates from other peers and flushing local changes — for as long as the
#' connection is open. Unlike [amsync_fetch()], which performs a one-off
#' retrieval over a throwaway connection, several documents can share a single
#' connection here.
#'
#' @inheritParams amsync_fetch
#' @param interval Interval in milliseconds for pushing local changes to
#'   the server. Default 1000. Uses [later::later()] to periodically check
#'   for and send local changes for every open document. This is a cheap no-op
#'   when there are no changes.
#'
#' @return An environment of class `"amsync_client"` with reference semantics,
#'   representing the connection:
#'   \describe{
#'     \item{`open_doc(doc_id, timeout)`}{Open a live document over this
#'       connection and return an `amsync_doc` handle for it (see below).
#'       Repeated calls for the same `doc_id` reuse the document already open
#'       on the connection rather than requesting it again.}
#'     \item{`close()`}{Disconnect and stop syncing all open documents.}
#'     \item{`active`}{Logical, whether the connection is active.}
#'   }
#'
#'   An `amsync_doc` handle returned by `$open_doc()` is itself an environment
#'   with:
#'   \describe{
#'     \item{`doc`}{The live automerge document, kept in sync with the server.}
#'     \item{`push()`}{Push this document's local changes to the server
#'       immediately.}
#'     \item{`close()`}{Stop syncing this one document (detach it from the
#'       connection); the connection and its other documents are unaffected.}
#'     \item{`active`}{Logical, whether the document is still open on an active
#'       connection.}
#'   }
#'
#' @details
#' Opening the connection performs a synchronous handshake before returning.
#' `$open_doc()` then performs a synchronous initial sync, so the returned
#' handle's `$doc` has meaningful content immediately. After that, incoming
#' changes are received asynchronously via a self-chaining promise loop, and
#' local changes are flushed periodically via a [later::later()] timer.
#'
#' Neither `close()` flushes pending local changes. Call `$push()` first if you
#' have unsynced edits — otherwise any changes made since the last
#' `sync`-interval tick may be lost.
#'
#' @examplesIf interactive()
#' server <- amsync_server()
#' server$start()
#' doc_id <- create_document(server)
#'
#' conn <- amsync_client(server$url)
#' doc <- conn$open_doc(doc_id)
#' automerge::am_keys(doc$doc)
#'
#' # Make local changes and push
#' automerge::am_put(doc$doc, automerge::AM_ROOT, "key", "value")
#' doc$push()
#'
#' # Open another document over the same connection
#' other <- conn$open_doc(create_document(server))
#'
#' # Disconnect (closes every document on the connection)
#' conn$close()
#' server$close()
#'
#' @export
amsync_client <- function(
  url,
  timeout = 5000L,
  tls = NULL,
  token = NULL,
  interval = 1000L
) {
  peer_id <- generate_peer_id()

  s <- stream(
    dial = url,
    tls = tls,
    textframes = FALSE,
    headers = bearer_headers(token)
  )
  on.exit(close(s))

  # --- Synchronous handshake ---

  send_msg(s, join_msg(peer_id))

  peer_raw <- recv_msg(s, timeout = timeout)

  if (inherits(peer_raw, "errorValue")) {
    stop("Failed to receive peer response: ", peer_raw)
  }

  peer_msg <- cbordec(peer_raw)
  if (peer_msg$type == "error") {
    stop("Server error: ", peer_msg$message)
  }
  if (peer_msg$type != "peer") {
    stop("Expected peer message, got: ", peer_msg$type)
  }

  server_peer_id <- peer_msg$senderId

  # --- Connection state ---

  client <- new.env(parent = emptyenv())
  client$active <- TRUE
  client$stream <- s
  client$url <- url
  client$peer_id <- peer_id
  client$server_peer_id <- server_peer_id
  client$interval <- interval
  client$timeout <- timeout
  client$sync_timer <- NULL
  client$last_error <- NULL
  # Per-document registry: doc_id -> environment holding the live document, its
  # sync state, and open-sync bookkeeping. Documents are added by open_doc().
  client$documents <- new.env(parent = emptyenv())

  # Register a document on the connection, returning its registry entry.
  register_doc <- function(doc_id, doc, sync_state) {
    entry <- new.env(parent = emptyenv())
    entry$doc_id <- doc_id
    entry$doc <- doc
    entry$sync_state <- sync_state
    entry$received <- 0L
    entry$last_activity <- Sys.time()
    entry$unavailable <- FALSE
    client$documents[[doc_id]] <- entry
    entry
  }

  # Send a sync message for one document's local changes.
  send_sync_for <- function(entry) {
    sync_data <- am_sync_encode(entry$doc, entry$sync_state)
    if (!is.null(sync_data)) {
      send_msg(
        client$stream,
        sync_msg(
          "sync",
          client$peer_id,
          client$server_peer_id,
          entry$doc_id,
          sync_data
        )
      )
    }
  }

  # Process an incoming message, routing sync/doc-unavailable to the matching
  # document by its id. Messages for unknown documents are ignored.
  process_message <- function(raw_data) {
    msg <- tryCatch(cbordec(raw_data), error = function(e) NULL)
    if (is.null(msg)) {
      return()
    }

    if (msg$type == "sync" && !is.null(msg$documentId)) {
      entry <- client$documents[[msg$documentId]]
      if (!is.null(entry)) {
        entry$received <- entry$received + 1L
        entry$last_activity <- Sys.time()
        apply_sync_and_reply(
          client$stream,
          entry$doc,
          entry$sync_state,
          msg$data,
          client$peer_id,
          client$server_peer_id,
          entry$doc_id
        )
      }
    } else if (msg$type == "doc-unavailable") {
      if (!is.null(msg$documentId)) {
        entry <- client$documents[[msg$documentId]]
        if (!is.null(entry)) {
          entry$unavailable <- TRUE
        }
      }
      warning("Document not available on server: ", msg$documentId %||% "unknown")
    } else if (msg$type == "error") {
      client$last_error <- msg$message
      warning("Server error: ", msg$message)
    }
  }

  # --- Async receive loop (self-chaining promises) ---

  recv_loop <- function() {
    if (!client$active) {
      return()
    }
    aio <- recv_aio(client$stream, mode = "raw")
    # Track the pending aio so close() can settle it before tearing down the
    # stream, leaving no stale promise continuation for a later run_now to hit.
    client$recv_aio <- aio
    promises::then(
      aio,
      onFulfilled = function(value) {
        tryCatch(
          process_message(value),
          error = function(e) {
            warning("[CLIENT] Receive error: ", conditionMessage(e))
          }
        )
        recv_loop()
      },
      onRejected = function(err) {
        client$active <- FALSE
      }
    )
    invisible()
  }

  # --- Periodic sync loop (outgoing local changes for every open document) ---

  sync_loop <- function() {
    if (!client$active) {
      return()
    }
    client$sync_timer <- later(
      function() {
        if (!client$active) {
          return()
        }
        tryCatch(
          for (id in ls(client$documents)) {
            send_sync_for(client$documents[[id]])
          },
          error = function(e) {
            client$active <- FALSE
            close(client$stream)
          }
        )
        sync_loop()
      },
      delay = interval / 1000
    )
  }

  # Build the public handle for a document registry entry.
  make_doc_handle <- function(entry) {
    handle <- new.env(parent = emptyenv())
    handle$doc_id <- entry$doc_id
    handle$doc <- entry$doc
    handle$sync_state <- entry$sync_state
    handle$stream <- client$stream
    handle$connection <- client
    handle$push <- function() send_sync_for(entry)
    handle$close <- function() {
      if (exists(entry$doc_id, envir = client$documents, inherits = FALSE)) {
        rm(list = entry$doc_id, envir = client$documents)
      }
      invisible()
    }
    # `active` tracks both the connection and whether this document is still
    # attached, so a handle whose document was closed reports inactive.
    makeActiveBinding(
      "active",
      function() {
        isTRUE(client$active) && !is.null(client$documents[[entry$doc_id]])
      },
      handle
    )
    class(handle) <- "amsync_doc"
    handle
  }

  # --- Open a live document over the connection ---

  open_doc <- function(doc_id, timeout = client$timeout) {
    if (!isTRUE(client$active)) {
      stop("Connection is not active; reconnect with amsync_client()")
    }
    existing <- client$documents[[doc_id]]
    if (!is.null(existing)) {
      return(make_doc_handle(existing))
    }

    entry <- register_doc(doc_id, am_create(), am_sync_state())
    client$last_error <- NULL

    sync_data <- am_sync_encode(entry$doc, entry$sync_state)
    send_msg(
      client$stream,
      sync_msg(
        "request",
        client$peer_id,
        client$server_peer_id,
        doc_id,
        sync_data %||% raw(0)
      )
    )

    # Drive the shared event loop until the initial sync settles. Completion is
    # detected per-document: at least one sync received, then a short idle.
    deadline <- Sys.time() + timeout / 1000
    idle_secs <- 0.5
    repeat {
      run_now(0.05)

      if (!is.null(client$last_error)) {
        rm(list = doc_id, envir = client$documents)
        stop("Server error: ", client$last_error)
      }
      if (entry$unavailable) {
        rm(list = doc_id, envir = client$documents)
        stop("Document not available on server: ", doc_id)
      }
      if (!isTRUE(client$active)) {
        rm(list = doc_id, envir = client$documents)
        stop("Connection closed before document sync completed")
      }

      now <- Sys.time()
      idle <- as.numeric(now - entry$last_activity, units = "secs")
      if (entry$received > 0L && idle > idle_secs) {
        break
      }
      if (now > deadline) {
        if (entry$received == 0L) {
          rm(list = doc_id, envir = client$documents)
          stop("No sync response from server within ", timeout, "ms")
        }
        break
      }
    }

    make_doc_handle(entry)
  }

  # --- Methods ---

  client$open_doc <- open_doc

  client$close <- function() {
    if (!client$active) {
      return(invisible())
    }
    client$active <- FALSE
    if (!is.null(client$sync_timer)) {
      client$sync_timer()
      client$sync_timer <- NULL
    }
    # Settle the pending receive aio and flush its (now active == FALSE, so
    # non-rescheduling) continuation before closing the stream. This prevents a
    # stale aio external pointer from lingering in the event loop and being
    # tripped over by a later run_now after garbage collection.
    if (!is.null(client$recv_aio)) {
      stop_aio(client$recv_aio)
      for (i in seq_len(3L)) run_now()
      client$recv_aio <- NULL
    }
    close(client$stream)
    invisible()
  }

  class(client) <- "amsync_client"
  on.exit() # stream now owned by client$close

  # Start the async loops
  recv_loop()
  sync_loop()

  client
}

#' @export
print.amsync_client <- function(x, ...) {
  cat("Automerge Sync Connection\n")
  cat("  Server:", x$url, "\n")
  cat("  Documents:", length(x$documents), "\n")
  cat("  Active:", x$active, "\n")
  invisible(x)
}

#' @export
print.amsync_doc <- function(x, ...) {
  cat("Automerge Document\n")
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
