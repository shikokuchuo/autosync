# Outbound peer connection for server-to-server sync

#' Connect to a peer server
#'
#' Establishes a persistent WebSocket connection to a remote autosync server,
#' performing the join/peer handshake and syncing all local documents. Incoming
#' sync messages (including announced documents from the remote) are applied
#' to the local server state.
#'
#' @param state Server state environment.
#' @param url WebSocket URL of the remote peer server.
#' @param tls Optional TLS configuration for the outbound connection.
#'
#' @noRd
connect_peer <- function(state, url, tls = NULL) {
  peer_id <- state$peer_id

  s <- tryCatch(
    stream(dial = url, tls = tls, textframes = FALSE),
    error = function(e) {
      warning("Failed to connect to peer ", url, ": ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(s)) return(invisible())

  # Send join with isPeer flag
  join <- list(
    type = "join",
    senderId = peer_id,
    peerMetadata = list(
      isPeer = TRUE,
      storageId = state$storage_id,
      isEphemeral = is.null(state$storage_id)
    ),
    supportedProtocolVersions = list("1")
  )
  send(s, cborenc(join), mode = "raw", block = TRUE)

  # Receive peer response
  aio <- recv_aio(s, mode = "raw", timeout = 5000L)
  while (unresolved(aio)) {
    run_now(1L)
  }
  peer_raw <- aio$data

  if (inherits(peer_raw, "errorValue")) {
    warning("Peer handshake timeout with ", url)
    close(s)
    return(invisible())
  }

  peer_msg <- tryCatch(cbordec(peer_raw), error = function(e) NULL)
  if (is.null(peer_msg) || peer_msg$type == "error") {
    warning(
      "Peer handshake failed with ", url, ": ",
      if (!is.null(peer_msg)) peer_msg$message else "decode error"
    )
    close(s)
    return(invisible())
  }

  if (peer_msg$type != "peer") {
    warning("Expected peer message from ", url, ", got: ", peer_msg$type)
    close(s)
    return(invisible())
  }

  remote_peer_id <- peer_msg$senderId

  # Wrap the stream with a $send() method matching the ws handler interface
  ws <- new.env(hash = TRUE, parent = emptyenv())
  ws$stream <- s
  ws$send <- function(data) send(s, data, mode = "raw", block = TRUE)

  # Register the peer connection in server state
  conn <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = peer_msg$peerMetadata %||% list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )
  state$connections[[remote_peer_id]] <- conn

  # Send request for all local documents
  for (doc_id in ls(state$documents)) {
    doc <- state$documents[[doc_id]]

    # Create sync state for this peer/document pair
    client_states <- state$sync_states[[remote_peer_id]]
    if (is.null(client_states)) {
      client_states <- new.env(hash = TRUE, parent = emptyenv())
      state$sync_states[[remote_peer_id]] <- client_states
    }
    if (is.null(client_states[[doc_id]])) {
      client_states[[doc_id]] <- am_sync_state()
      add_doc_peer(state, doc_id, remote_peer_id)
    }

    sync_state <- client_states[[doc_id]]
    sync_data <- tryCatch(am_sync_encode(doc, sync_state), error = function(e) NULL)

    request <- list(
      type = "request",
      senderId = peer_id,
      targetId = remote_peer_id,
      documentId = doc_id,
      data = sync_data %||% raw(0)
    )
    ws$send(cborenc(request))
  }

  # Start async receive loop for incoming messages from the peer
  peer_recv_loop(state, s, remote_peer_id)

  invisible()
}

#' Async receive loop for a peer connection
#'
#' Schedules asynchronous receives on the peer stream, processing incoming
#' messages and re-scheduling until the connection closes.
#'
#' @param state Server state environment.
#' @param s Stream connection to the peer.
#' @param remote_peer_id The remote peer's ID.
#'
#' @noRd
peer_recv_loop <- function(state, s, remote_peer_id) {
  aio <- recv_aio(s, mode = "raw", timeout = 5000L)

  later(
    function() {
      if (unresolved(aio)) {
        # Still waiting, re-schedule
        peer_recv_loop(state, s, remote_peer_id)
        return()
      }

      result <- aio$data
      if (inherits(result, "errorValue")) {
        # Connection closed or error - clean up
        handle_disconnect(state, remote_peer_id)
        if (exists(remote_peer_id, envir = state$connections)) {
          rm(list = remote_peer_id, envir = state$connections)
        }
        tryCatch(close(s), error = function(e) NULL)
        return()
      }

      # Process the message
      msg <- tryCatch(cbordec(result), error = function(e) NULL)
      if (!is.null(msg)) {
        handle_peer_message(state, remote_peer_id, msg)
      }

      # Schedule next receive
      peer_recv_loop(state, s, remote_peer_id)
    },
    delay = 0.1
  )
}

#' Handle an incoming message from a peer connection
#'
#' Processes sync and other messages received from a peer server.
#'
#' @param state Server state environment.
#' @param remote_peer_id The remote peer's ID.
#' @param msg Decoded CBOR message.
#'
#' @noRd
handle_peer_message <- function(state, remote_peer_id, msg) {
  if (msg$type == "sync") {
    doc_id <- msg$documentId
    if (is.null(doc_id)) return(invisible())

    # Get or create document
    doc <- state$documents[[doc_id]]
    if (is.null(doc)) {
      if (state$auto_create_docs) {
        doc <- am_create()
        state$documents[[doc_id]] <- doc
      } else {
        return(invisible())
      }
    }

    # Get or create sync state
    client_states <- state$sync_states[[remote_peer_id]]
    if (is.null(client_states)) {
      client_states <- new.env(hash = TRUE, parent = emptyenv())
      state$sync_states[[remote_peer_id]] <- client_states
    }
    if (is.null(client_states[[doc_id]])) {
      client_states[[doc_id]] <- am_sync_state()
      add_doc_peer(state, doc_id, remote_peer_id)
    }

    sync_state <- client_states[[doc_id]]

    if (length(msg$data)) {
      tryCatch(
        am_sync_decode(doc, sync_state, msg$data),
        error = function(e) {
          warning("Peer sync decode error: ", conditionMessage(e))
        }
      )
      save_document(state, doc_id, doc)
    }

    # Send reply if we have data
    reply_data <- tryCatch(
      am_sync_encode(doc, sync_state),
      error = function(e) NULL
    )
    if (!is.null(reply_data)) {
      response <- list(
        type = "sync",
        senderId = state$peer_id,
        targetId = remote_peer_id,
        documentId = doc_id,
        data = reply_data
      )
      send_to_peer(state, remote_peer_id, response)
    }

    # Broadcast to other peers/clients
    broadcast_sync(state, remote_peer_id, doc_id, doc)
  }

  invisible()
}
