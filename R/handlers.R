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
  if (
    !is.null(msg$supportedProtocolVersions) &&
      as.integer(msg$supportedProtocolVersions) != 1L
  ) {
    send_error(
      server,
      msg$senderId,
      "Unsupported protocol version",
      temp_id = temp_id
    )
    return(invisible())
  }

  # Close previous connection from same client (reconnection)
  client_id <- msg$senderId
  if (exists(client_id, envir = server$connections, inherits = FALSE)) {
    old_conn <- server$connections[[client_id]]
    if (
      !is.null(old_conn$ws) &&
        !identical(old_conn$ws, server$connections[[temp_id]]$ws)
    ) {
      old_temp_id <- as.character(old_conn$ws$id)
      handle_disconnect(server, client_id)
      if (exists(old_temp_id, envir = server$connections, inherits = FALSE)) {
        rm(list = old_temp_id, envir = server$connections)
      }
      if (exists(client_id, envir = server$connections, inherits = FALSE)) {
        rm(list = client_id, envir = server$connections)
      }
      old_conn$ws$close()
    }
  }

  # Authentication check
  if (!is.null(server$auth)) {
    # Clear pending auth tracker - client sent join message in time
    if (exists(temp_id, envir = server$pending_auth, inherits = FALSE)) {
      rm(list = temp_id, envir = server$pending_auth)
    }

    auth_result <- authenticate_client(server$auth, msg$peerMetadata)

    if (!auth_result$valid) {
      send_error(server, msg$senderId, auth_result$error, temp_id = temp_id)
      close_connection(server, temp_id)
      return(invisible())
    }

    # Store authenticated email for logging/access control
    server$connections[[temp_id]]$authenticated_email <- auth_result$email
  }

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
    return(invisible())
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
      doc <- am_create()
      server$documents[[doc_id]] <- doc
    } else {
      send_unavailable(server, client_id, doc_id)
      return(invisible())
    }
  }

  # Get or create sync state for this client/document pair
  # Uses nested envs: sync_states[[client_id]][[doc_id]]
  client_states <- server$sync_states[[client_id]]
  if (is.null(client_states)) {
    client_states <- new.env(hash = TRUE, parent = emptyenv())
    server$sync_states[[client_id]] <- client_states
  }
  sync_state <- client_states[[doc_id]]
  if (is.null(sync_state)) {
    sync_state <- am_sync_state()
    client_states[[doc_id]] <- sync_state
    add_doc_peer(server, doc_id, client_id)
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
#' Forwards ephemeral messages without persistence. Supports both:
#' - Point-to-point: message with targetId forwards to specific peer
#' - Broadcast: message with documentId relays to all peers on that document
#'
#' @param server An amsync_server object.
#' @param client_id Client's peer ID.
#' @param msg Decoded ephemeral message.
#'
#' @noRd
handle_ephemeral <- function(server, client_id, msg) {
  target_id <- msg$targetId
  doc_id <- msg$documentId

  if (!is.null(target_id) && exists(target_id, envir = server$connections)) {
    send_to_peer(server, target_id, msg)
  } else if (!is.null(doc_id)) {
    broadcast_ephemeral(server, client_id, doc_id, msg)
  }
  invisible()
}

#' Add a client to a document's peer list
#'
#' @param server An amsync_server object.
#' @param doc_id Document ID.
#' @param client_id Client's peer ID.
#'
#' @noRd
add_doc_peer <- function(server, doc_id, client_id) {
  peers <- server$doc_peers[[doc_id]]
  if (is.null(peers)) {
    server$doc_peers[[doc_id]] <- client_id
  } else if (!client_id %in% peers) {
    server$doc_peers[[doc_id]] <- c(peers, client_id)
  }
  invisible()
}

#' Remove a client from a document's peer list
#'
#' @param server An amsync_server object.
#' @param doc_id Document ID.
#' @param client_id Client's peer ID.
#'
#' @noRd
remove_doc_peer <- function(server, doc_id, client_id) {
  peers <- server$doc_peers[[doc_id]]
  if (!is.null(peers)) {
    peers <- peers[peers != client_id]
    if (length(peers) == 0L) {
      rm(list = doc_id, envir = server$doc_peers)
    } else {
      server$doc_peers[[doc_id]] <- peers
    }
  }
  invisible()
}

#' Remove a client from all document peer lists
#'
#' @param server An amsync_server object.
#' @param client_id Client's peer ID.
#'
#' @noRd
remove_peer_from_all_docs <- function(server, client_id) {
  for (doc_id in ls(server$doc_peers)) {
    remove_doc_peer(server, doc_id, client_id)
  }
  invisible()
}

#' Broadcast ephemeral message to all peers on a document
#'
#' @param server An amsync_server object.
#' @param sender_client_id ID of the client who sent the message.
#' @param doc_id Document ID.
#' @param msg The ephemeral message to broadcast.
#'
#' @noRd
broadcast_ephemeral <- function(server, sender_client_id, doc_id, msg) {
  peers <- server$doc_peers[[doc_id]]
  if (is.null(peers)) {
    return(invisible())
  }

  for (client_id in peers) {
    if (client_id == sender_client_id) {
      next
    }

    relay_msg <- msg
    relay_msg$targetId <- client_id
    send_to_peer(server, client_id, relay_msg)
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
  if (exists(client_id, envir = server$sync_states)) {
    rm(list = client_id, envir = server$sync_states)
  }
  remove_peer_from_all_docs(server, client_id)

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
  peers <- server$doc_peers[[doc_id]]
  if (is.null(peers)) {
    return(invisible())
  }

  for (client_id in peers) {
    if (client_id == sender_client_id) {
      next
    }

    sync_state <- server$sync_states[[client_id]][[doc_id]]

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
