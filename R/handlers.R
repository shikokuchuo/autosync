# Message handlers for autosync

#' Main message dispatcher
#'
#' Routes incoming CBOR messages to the appropriate handler.
#'
#' @param server An amsync_server object.
#' @param client_id Client's peer ID (or temp_id if pre-handshake).
#' @param temp_id Temporary connection ID.
#' @param raw_msg Raw CBOR message bytes.
#'
#' @noRd
handle_message <- function(server, client_id, temp_id, raw_msg) {
  msg <- tryCatch(
    cbordec(raw_msg),
    error = function(e) {
      warning("CBOR decode error from ", client_id, ": ", conditionMessage(e))
      return(NULL)
    }
  )
  if (is.null(msg)) {
    return(invisible())
  }

  switch(
    msg$type,
    "join" = handle_join(server, temp_id, msg),
    "request" = handle_sync(server, client_id, msg, is_request = TRUE),
    "sync" = handle_sync(server, client_id, msg, is_request = FALSE),
    "leave" = handle_leave(server, client_id, msg),
    "ephemeral" = handle_ephemeral(server, client_id, msg),
    "error" = handle_error(server, client_id, msg),
    warning("Unknown message type: ", msg$type)
  )

  invisible()
}

#' Handle join handshake message
#'
#' @param server An amsync_server object.
#' @param temp_id Temporary connection ID.
#' @param msg Decoded join message.
#'
#' @noRd
handle_join <- function(server, temp_id, msg) {
  # Validate protocol version
  # Note: CBOR arrays are decoded as R lists, so use unlist() for safe comparison
  if (!"1" %in% unlist(msg$supportedProtocolVersions)) {
    send_error(
      server,
      msg$senderId,
      "Unsupported protocol version",
      temp_id = temp_id
    )
    return(invisible())
  }

  # Get the client's senderId - this becomes their canonical identifier
  client_id <- msg$senderId

  # Update connection: store client_id and metadata
  # Must update the original entry (R lists are copied, not referenced)
  server$connections[[temp_id]]$client_id <- client_id
  server$connections[[temp_id]]$metadata <- msg$peerMetadata %||% list()

  # Also store connection indexed by client_id for message routing
  server$connections[[client_id]] <- server$connections[[temp_id]]

  response <- list(
    type = "peer",
    senderId = server$peer_id,
    targetId = client_id,
    peerMetadata = if (is.null(server$storage_id)) {
      list(isEphemeral = TRUE)
    } else {
      list(storageId = server$storage_id, isEphemeral = FALSE)
    },
    selectedProtocolVersion = "1"
  )
  send_to_peer(server, client_id, response)

  invisible()
}

#' Handle sync or request message
#'
#' @param server An amsync_server object.
#' @param client_id Client's peer ID.
#' @param msg Decoded sync/request message.
#' @param is_request Logical, TRUE for request messages.
#'
#' @noRd
handle_sync <- function(server, client_id, msg, is_request) {
  # Validate targetId matches this server
  if (!is.null(msg$targetId) && msg$targetId != server$peer_id) {
    return(invisible()) # Message not intended for us
  }

  doc_id <- msg$documentId

  # Validate document ID format (base58check)
  doc_bytes <- tryCatch(
    base58dec(doc_id, convert = FALSE),
    error = function(e) NULL
  )
  if (is.null(doc_bytes)) {
    send_error(server, client_id, "Invalid document ID format")
    return(invisible())
  }

  # Get or create document based on server policy
  doc <- server$documents[[doc_id]]

  if (is.null(doc)) {
    if (server$auto_create_docs) {
      # New document - create it (if auto-creation enabled)
      doc <- am_create()
      server$documents[[doc_id]] <- doc
    } else {
      # Unknown document, send unavailable
      send_unavailable(server, client_id, doc_id)
      return(invisible())
    }
  }

  # Get or create sync state for this client/document pair
  # Key uses client_id (the client's senderId from join)
  state_key <- paste(client_id, doc_id, sep = ":")
  sync_state <- server$sync_states[[state_key]]
  if (is.null(sync_state)) {
    sync_state <- am_sync_state_new()
    server$sync_states[[state_key]] <- sync_state
  }

  if (length(msg$data)) {
    tryCatch(
      am_sync_decode(doc, sync_state, msg$data),
      error = function(e) {
        warning("am_sync_decode error: ", conditionMessage(e))
      }
    )
    save_document(server, doc_id, doc)
  }

  reply_data <- tryCatch(
    am_sync_encode(doc, sync_state),
    error = function(e) NULL
  )
  if (!is.null(reply_data)) {
    response <- list(
      type = "sync",
      senderId = server$peer_id,
      targetId = client_id,
      documentId = doc_id,
      data = reply_data
    )
    send_to_peer(server, client_id, response)
  }

  broadcast_sync(server, client_id, doc_id, doc)

  invisible()
}

#' Handle leave message
#'
#' @param server An amsync_server object.
#' @param client_id Client's peer ID.
#' @param msg Decoded leave message.
#'
#' @noRd
handle_leave <- function(server, client_id, msg) {
  # Client is disconnecting gracefully
  # Clean up will happen in onClose handler
  invisible()
}

#' Handle ephemeral message
#'
#' Forwards ephemeral messages to the target peer without persistence.
#'
#' @param server An amsync_server object.
#' @param client_id Client's peer ID.
#' @param msg Decoded ephemeral message.
#'
#' @noRd
handle_ephemeral <- function(server, client_id, msg) {
  target_id <- msg$targetId
  if (!is.null(target_id) && exists(target_id, envir = server$connections)) {
    send_to_peer(server, target_id, msg)
  }
  invisible()
}

#' Handle error message from client
#'
#' @param server An amsync_server object.
#' @param client_id Client's peer ID.
#' @param msg Decoded error message.
#'
#' @noRd
handle_error <- function(server, client_id, msg) {
  warning("Error from client ", client_id, ": ", msg$message)
  invisible()
}

#' Clean up resources when client disconnects
#'
#' @param server An amsync_server object.
#' @param client_id Client's peer ID.
#'
#' @noRd
handle_disconnect <- function(server, client_id) {
  if (is.null(client_id)) {
    return(invisible())
  }
  all_keys <- ls(server$sync_states)
  client_keys <- all_keys[startsWith(all_keys, paste0(client_id, ":"))]
  if (length(client_keys) > 0L) {
    rm(list = client_keys, envir = server$sync_states)
  }

  invisible()
}

#' Send message to specific peer
#'
#' @param server An amsync_server object.
#' @param peer_id Target peer ID.
#' @param msg List to encode and send.
#'
#' @noRd
send_to_peer <- function(server, peer_id, msg) {
  conn <- server$connections[[peer_id]]
  if (!is.null(conn) && !is.null(conn$ws)) {
    raw_msg <- cborenc(msg)
    conn$ws$send(raw_msg)
  }
  invisible()
}

#' Send error message to peer
#'
#' @param server An amsync_server object.
#' @param peer_id Target peer ID.
#' @param message Error message string.
#' @param temp_id Optional temporary connection ID for pre-handshake errors.
#'
#' @noRd
send_error <- function(server, peer_id, message, temp_id = NULL) {
  response <- list(
    type = "error",
    senderId = server$peer_id,
    targetId = peer_id,
    message = message
  )
  if (!is.null(temp_id)) {
    conn <- server$connections[[temp_id]]
    if (!is.null(conn) && !is.null(conn$ws)) {
      conn$ws$send(cborenc(response))
    }
  } else {
    send_to_peer(server, peer_id, response)
  }
  invisible()
}

#' Send doc-unavailable message
#'
#' @param server An amsync_server object.
#' @param client_id Client's peer ID.
#' @param doc_id Document ID.
#'
#' @noRd
send_unavailable <- function(server, client_id, doc_id) {
  response <- list(
    type = "doc-unavailable",
    senderId = server$peer_id,
    targetId = client_id,
    documentId = doc_id
  )
  send_to_peer(server, client_id, response)
  invisible()
}

#' Broadcast sync changes to all connected peers
#'
#' @param server An amsync_server object.
#' @param sender_client_id ID of the client who sent the change.
#' @param doc_id Document ID.
#' @param doc Automerge document object.
#'
#' @noRd
broadcast_sync <- function(server, sender_client_id, doc_id, doc) {
  for (cid in ls(server$connections)) {
    conn <- server$connections[[cid]]

    # Skip: sender, pre-handshake connections, and duplicate entries (temp_id)
    if (is.null(conn$client_id)) {
      next
    }
    if (conn$client_id == sender_client_id) {
      next
    }
    if (cid != conn$client_id) {
      next
    } # Skip temp_id entries

    client_id <- conn$client_id

    # Check if this client has a sync state for this document
    state_key <- paste(client_id, doc_id, sep = ":")
    sync_state <- server$sync_states[[state_key]]
    if (is.null(sync_state)) {
      next
    }

    reply_data <- am_sync_encode(doc, sync_state)
    if (!is.null(reply_data)) {
      response <- list(
        type = "sync",
        senderId = server$peer_id,
        targetId = client_id,
        documentId = doc_id,
        data = reply_data
      )
      send_to_peer(server, client_id, response)
    }
  }

  invisible()
}
