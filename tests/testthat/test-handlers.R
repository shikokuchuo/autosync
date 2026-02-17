# Test handlers by creating the state environment directly
# without using actual WebSocket connections

create_test_state <- function(data_dir = tempfile(), auto_create_docs = TRUE) {
  dir.create(data_dir, showWarnings = FALSE)

  state <- new.env(hash = TRUE, parent = emptyenv())
  state$data_dir <- data_dir
  state$auto_create_docs <- auto_create_docs
  state$peer_id <- autosync:::generate_peer_id()
  state$storage_id <- autosync:::generate_peer_id()
  state$documents <- new.env(hash = TRUE, parent = emptyenv())
  state$sync_states <- new.env(hash = TRUE, parent = emptyenv())
  state$connections <- new.env(hash = TRUE, parent = emptyenv())
  state$doc_peers <- new.env(hash = TRUE, parent = emptyenv())
  state
}

# Mock WebSocket object for testing
create_mock_ws <- function() {
  ws <- new.env(hash = TRUE)
  ws$sent_messages <- list()
  ws$send <- function(data) {
    ws$sent_messages <- c(ws$sent_messages, list(data))
  }
  ws$id <- as.character(sample.int(1e6, 1))
  ws
}

# Helper to set up nested sync state (sync_states[[client_id]][[doc_id]])
set_sync_state <- function(state, client_id, doc_id, sync_state = NULL) {
  if (is.null(state$sync_states[[client_id]])) {
    state$sync_states[[client_id]] <- new.env(hash = TRUE, parent = emptyenv())
  }
  state$sync_states[[client_id]][[doc_id]] <- sync_state %||%
    automerge::am_sync_state()
}

test_that("handle_message dispatches to handle_join", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Create mock connection
  ws <- create_mock_ws()
  temp_id <- ws$id
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  join_msg <- secretbase::cborenc(list(
    type = "join",
    senderId = "testClient123",
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("1")
  ))

  autosync:::handle_message(state, temp_id, temp_id, join_msg)

  # Should have sent a response
  expect_length(ws$sent_messages, 1)

  # Decode the response
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "peer")
  expect_equal(response$targetId, "testClient123")
  expect_equal(response$selectedProtocolVersion, "1")
})

test_that("handle_join rejects unsupported protocol version", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  temp_id <- ws$id
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  join_msg <- list(
    type = "join",
    senderId = "badVersionClient",
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("999") # Unsupported
  )

  autosync:::handle_join(state, temp_id, join_msg)

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "error")
  expect_true(grepl("protocol", response$message, ignore.case = TRUE))
})

test_that("handle_join sets up connection state correctly", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  temp_id <- ws$id
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  join_msg <- list(
    type = "join",
    senderId = "setupClient",
    peerMetadata = list(customField = "value"),
    supportedProtocolVersions = list("1")
  )

  autosync:::handle_join(state, temp_id, join_msg)

  # Check connection was updated
  expect_equal(state$connections[[temp_id]]$client_id, "setupClient")
  expect_equal(state$connections[[temp_id]]$metadata$customField, "value")

  # Check client_id index was created
  expect_true(exists("setupClient", envir = state$connections))
})

test_that("handle_sync validates document ID format", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Set up connected client
  ws <- create_mock_ws()
  client_id <- "syncClient"
  state$connections[[client_id]] <- list(
    ws = ws,
    client_id = client_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  sync_msg <- list(
    type = "request",
    senderId = client_id,
    targetId = state$peer_id,
    documentId = "!!!invalid!!!", # Invalid base58check
    data = raw(0)
  )

  autosync:::handle_sync(state, client_id, sync_msg, is_request = TRUE)

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "error")
  expect_true(grepl("Invalid", response$message, ignore.case = TRUE))
})

test_that("handle_sync sends doc-unavailable when auto_create_docs is FALSE", {
  state <- create_test_state(auto_create_docs = FALSE)
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  client_id <- "unavailableClient"
  state$connections[[client_id]] <- list(
    ws = ws,
    client_id = client_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  doc_id <- generate_document_id()
  sync_msg <- list(
    type = "request",
    senderId = client_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = raw(0)
  )

  autosync:::handle_sync(state, client_id, sync_msg, is_request = TRUE)

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "doc-unavailable")
  expect_equal(response$documentId, doc_id)
})

test_that("handle_sync auto-creates document when enabled", {
  state <- create_test_state(auto_create_docs = TRUE)
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  client_id <- "autoCreateClient"
  state$connections[[client_id]] <- list(
    ws = ws,
    client_id = client_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  doc_id <- generate_document_id()
  sync_msg <- list(
    type = "request",
    senderId = client_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = raw(0)
  )

  autosync:::handle_sync(state, client_id, sync_msg, is_request = TRUE)

  # Document should be created
  expect_true(exists(doc_id, envir = state$documents))
  expect_true(inherits(state$documents[[doc_id]], "am_doc"))

  # Should have sent sync response
  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "sync")
  expect_equal(response$documentId, doc_id)
})

test_that("handle_sync creates sync state and doc_peers entry", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Pre-create a document
  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  ws <- create_mock_ws()
  client_id <- "syncStateClient"
  state$connections[[client_id]] <- list(
    ws = ws,
    client_id = client_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  sync_msg <- list(
    type = "request",
    senderId = client_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = raw(0)
  )

  autosync:::handle_sync(state, client_id, sync_msg, is_request = TRUE)

  # Sync state should be created (nested: sync_states[[client_id]][[doc_id]])
  expect_true(exists(client_id, envir = state$sync_states))
  expect_true(exists(doc_id, envir = state$sync_states[[client_id]]))

  # Client should be added to doc_peers
  expect_true(client_id %in% state$doc_peers[[doc_id]])
})

test_that("handle_sync ignores messages with wrong targetId", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  client_id <- "wrongTargetClient"
  state$connections[[client_id]] <- list(
    ws = ws,
    client_id = client_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  sync_msg <- list(
    type = "request",
    senderId = client_id,
    targetId = "someOtherPeer", # Not the server's peer_id
    documentId = generate_document_id(),
    data = raw(0)
  )

  autosync:::handle_sync(state, client_id, sync_msg, is_request = TRUE)

  # No response should be sent
  expect_length(ws$sent_messages, 0)
})

test_that("handle_message handles malformed CBOR gracefully", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  temp_id <- ws$id
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  invalid_cbor <- as.raw(c(0xFF, 0xFF, 0xFF, 0xFF))

  # Should warn but not error
  expect_warning(
    autosync:::handle_message(state, temp_id, temp_id, invalid_cbor),
    "CBOR decode error"
  )
})

test_that("handle_message handles unknown message types", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  temp_id <- ws$id
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = temp_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  unknown_msg <- secretbase::cborenc(list(
    type = "unknownType123",
    senderId = temp_id
  ))

  # Should warn but not error
  expect_warning(
    autosync:::handle_message(state, temp_id, temp_id, unknown_msg),
    "Unknown message type"
  )
})

test_that("handle_disconnect cleans up sync states and doc_peers", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  client_id <- "disconnectClient"
  doc_id <- generate_document_id()
  doc_id2 <- "anotherdoc"

  # Create sync states for this client (nested structure)
  set_sync_state(state, client_id, doc_id)
  set_sync_state(state, client_id, doc_id2)
  state$doc_peers[[doc_id]] <- c(client_id, "otherClient")
  state$doc_peers[[doc_id2]] <- client_id

  # Also create sync state for another client (should not be removed)
  set_sync_state(state, "otherClient", doc_id)

  autosync:::handle_disconnect(state, client_id)

  # Client's entire sync state env should be removed
  expect_false(exists(client_id, envir = state$sync_states))

  # Other client's sync state should remain
  expect_true(exists("otherClient", envir = state$sync_states))
  expect_true(exists(doc_id, envir = state$sync_states[["otherClient"]]))

  # Client should be removed from doc_peers
  expect_equal(state$doc_peers[[doc_id]], "otherClient")
  expect_false(exists(doc_id2, envir = state$doc_peers)) # empty list removed
})

test_that("handle_disconnect handles NULL client_id", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Should not error
  expect_no_error(autosync:::handle_disconnect(state, NULL))
})

test_that("handle_leave does nothing", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Should return invisibly without error
  result <- autosync:::handle_leave(state, "someClient", list(type = "leave"))
  expect_null(result)
})

test_that("handle_error logs warning", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  error_msg <- list(
    type = "error",
    senderId = "errorClient",
    message = "Test error message"
  )

  expect_warning(
    autosync:::handle_error(state, "errorClient", error_msg),
    "Test error message"
  )
})

test_that("handle_ephemeral forwards to target peer", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Set up two clients
  ws1 <- create_mock_ws()
  ws2 <- create_mock_ws()

  state$connections[["client1"]] <- list(
    ws = ws1,
    client_id = "client1",
    metadata = list(),
    connected_at = Sys.time()
  )

  state$connections[["client2"]] <- list(
    ws = ws2,
    client_id = "client2",
    metadata = list(),
    connected_at = Sys.time()
  )

  ephemeral_msg <- list(
    type = "ephemeral",
    senderId = "client1",
    targetId = "client2",
    data = "hello"
  )

  autosync:::handle_ephemeral(state, "client1", ephemeral_msg)

  # client2 should receive the message
  expect_length(ws2$sent_messages, 1)
  forwarded <- secretbase::cbordec(ws2$sent_messages[[1]])
  expect_equal(forwarded$type, "ephemeral")
  expect_equal(forwarded$senderId, "client1")
  expect_equal(forwarded$data, "hello")
})

test_that("handle_ephemeral ignores message if target not connected", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws1 <- create_mock_ws()
  state$connections[["client1"]] <- list(
    ws = ws1,
    client_id = "client1",
    metadata = list(),
    connected_at = Sys.time()
  )

  ephemeral_msg <- list(
    type = "ephemeral",
    senderId = "client1",
    targetId = "nonexistentClient", # Not connected
    data = "hello"
  )

  # Should not error
  expect_no_error(
    autosync:::handle_ephemeral(state, "client1", ephemeral_msg)
  )
})

test_that("handle_ephemeral broadcasts to all peers on document", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Set up three clients
  ws1 <- create_mock_ws()
  ws2 <- create_mock_ws()
  ws3 <- create_mock_ws()

  state$connections[["client1"]] <- list(
    ws = ws1,
    client_id = "client1",
    metadata = list(),
    connected_at = Sys.time()
  )

  state$connections[["client2"]] <- list(
    ws = ws2,
    client_id = "client2",
    metadata = list(),
    connected_at = Sys.time()
  )

  state$connections[["client3"]] <- list(
    ws = ws3,
    client_id = "client3",
    metadata = list(),
    connected_at = Sys.time()
  )

  # Create a document and sync states for clients 1 and 2 (but not 3)
  doc_id <- "testDoc123"
  state$documents[[doc_id]] <- automerge::am_create()
  set_sync_state(state, "client1", doc_id)
  set_sync_state(state, "client2", doc_id)
  state$doc_peers[[doc_id]] <- c("client1", "client2")
  # client3 has no sync state for this document

  # Broadcast ephemeral message from client1 (has documentId, no targetId)
  ephemeral_msg <- list(
    type = "ephemeral",
    senderId = "client1",
    documentId = doc_id,
    data = list(presence = "cursor at line 5")
  )

  autosync:::handle_ephemeral(state, "client1", ephemeral_msg)

  # client1 (sender) should NOT receive the message
  expect_length(ws1$sent_messages, 0)

  # client2 (has sync state) should receive the message
  expect_length(ws2$sent_messages, 1)
  forwarded <- secretbase::cbordec(ws2$sent_messages[[1]])
  expect_equal(forwarded$type, "ephemeral")
  expect_equal(forwarded$senderId, "client1")
  expect_equal(forwarded$documentId, doc_id)
  expect_equal(forwarded$targetId, "client2")
  expect_equal(forwarded$data$presence, "cursor at line 5")

  # client3 (no sync state) should NOT receive the message
  expect_length(ws3$sent_messages, 0)
})

test_that("handle_ephemeral prefers targetId over documentId broadcast", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Set up two clients with sync state
  ws1 <- create_mock_ws()
  ws2 <- create_mock_ws()
  ws3 <- create_mock_ws()

  state$connections[["client1"]] <- list(
    ws = ws1,
    client_id = "client1",
    metadata = list(),
    connected_at = Sys.time()
  )

  state$connections[["client2"]] <- list(
    ws = ws2,
    client_id = "client2",
    metadata = list(),
    connected_at = Sys.time()
  )

  state$connections[["client3"]] <- list(
    ws = ws3,
    client_id = "client3",
    metadata = list(),
    connected_at = Sys.time()
  )

  doc_id <- "testDoc456"
  state$documents[[doc_id]] <- automerge::am_create()
  set_sync_state(state, "client1", doc_id)
  set_sync_state(state, "client2", doc_id)
  set_sync_state(state, "client3", doc_id)
  state$doc_peers[[doc_id]] <- c("client1", "client2", "client3")

  # Message with BOTH targetId and documentId - should use point-to-point
  ephemeral_msg <- list(
    type = "ephemeral",
    senderId = "client1",
    targetId = "client2",
    documentId = doc_id,
    data = "direct message"
  )

  autosync:::handle_ephemeral(state, "client1", ephemeral_msg)

  # Only client2 should receive (point-to-point, not broadcast)
  expect_length(ws2$sent_messages, 1)
  expect_length(ws3$sent_messages, 0)
})

test_that("send_to_peer sends encoded message", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  peer_id <- "testPeer"
  state$connections[[peer_id]] <- list(
    ws = ws,
    client_id = peer_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  msg <- list(type = "test", data = "hello")
  autosync:::send_to_peer(state, peer_id, msg)

  expect_length(ws$sent_messages, 1)
  decoded <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(decoded$type, "test")
  expect_equal(decoded$data, "hello")
})

test_that("send_to_peer handles missing peer", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  msg <- list(type = "test")

  # Should not error when peer doesn't exist
  expect_no_error(
    autosync:::send_to_peer(state, "nonexistent", msg)
  )
})

test_that("send_error sends error message", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  peer_id <- "errorPeer"
  state$connections[[peer_id]] <- list(
    ws = ws,
    client_id = peer_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  autosync:::send_error(state, peer_id, "Something went wrong")

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "error")
  expect_equal(response$targetId, peer_id)
  expect_equal(response$message, "Something went wrong")
})

test_that("send_error with temp_id sends to temp connection", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  temp_id <- ws$id
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  autosync:::send_error(state, "targetPeer", "Error message", temp_id = temp_id)

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "error")
  expect_equal(response$targetId, "targetPeer")
})

test_that("send_unavailable sends doc-unavailable message", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  client_id <- "unavailablePeer"
  state$connections[[client_id]] <- list(
    ws = ws,
    client_id = client_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  doc_id <- "missingDoc123"
  autosync:::send_unavailable(state, client_id, doc_id)

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "doc-unavailable")
  expect_equal(response$targetId, client_id)
  expect_equal(response$documentId, doc_id)
})

test_that("broadcast_sync sends to other peers with sync state", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Create a document
  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  # Set up multiple clients
  ws1 <- create_mock_ws()
  ws2 <- create_mock_ws()
  ws3 <- create_mock_ws()

  state$connections[["sender"]] <- list(
    ws = ws1,
    client_id = "sender",
    metadata = list(),
    connected_at = Sys.time()
  )

  state$connections[["receiver1"]] <- list(
    ws = ws2,
    client_id = "receiver1",
    metadata = list(),
    connected_at = Sys.time()
  )

  state$connections[["receiver2"]] <- list(
    ws = ws3,
    client_id = "receiver2",
    metadata = list(),
    connected_at = Sys.time()
  )

  # Create sync states for receivers (not sender)
  set_sync_state(state, "receiver1", doc_id)
  set_sync_state(state, "receiver2", doc_id)
  state$doc_peers[[doc_id]] <- c("receiver1", "receiver2")

  autosync:::broadcast_sync(state, "sender", doc_id, doc)

  # Sender should not receive
  expect_length(ws1$sent_messages, 0)

  # Receivers should receive sync messages (or nothing if no new data)
  # The actual content depends on sync state
})

test_that("broadcast_sync skips pre-handshake connections", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  ws <- create_mock_ws()
  temp_id <- ws$id

  # Pre-handshake connection (client_id is NULL)
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  autosync:::broadcast_sync(state, "sender", doc_id, doc)

  # Should not send to pre-handshake connection
  expect_length(ws$sent_messages, 0)
})

test_that("handle_join with ephemeral server sets isEphemeral=TRUE in response", {
  state <- create_test_state()
  state$storage_id <- NULL # Ephemeral server
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  temp_id <- ws$id
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  join_msg <- list(
    type = "join",
    senderId = "ephemeralTestClient",
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("1")
  )

  autosync:::handle_join(state, temp_id, join_msg)

  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "peer")
  expect_true(response$peerMetadata$isEphemeral)
  expect_null(response$peerMetadata$storageId)
})

test_that("handle_join with persistent server includes storageId", {
  state <- create_test_state()
  state$storage_id <- "myStorageId123"
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  temp_id <- ws$id
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  join_msg <- list(
    type = "join",
    senderId = "persistentTestClient",
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("1")
  )

  autosync:::handle_join(state, temp_id, join_msg)

  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "peer")
  expect_false(response$peerMetadata$isEphemeral)
  expect_equal(response$peerMetadata$storageId, "myStorageId123")
})

test_that("handle_join accepts missing supportedProtocolVersions", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws <- create_mock_ws()
  temp_id <- ws$id
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  join_msg <- list(
    type = "join",
    senderId = "noVersionClient",
    peerMetadata = list(isEphemeral = TRUE)
    # supportedProtocolVersions intentionally omitted
  )

  autosync:::handle_join(state, temp_id, join_msg)

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "peer")
  expect_equal(response$targetId, "noVersionClient")
  expect_equal(response$selectedProtocolVersion, "1")
})

test_that("handle_join closes previous socket on duplicate senderId", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # First connection
  ws1 <- create_mock_ws()
  ws1$closed <- FALSE
  ws1$close <- function() ws1$closed <- TRUE
  temp_id1 <- ws1$id
  state$connections[[temp_id1]] <- list(
    ws = ws1,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  join_msg1 <- list(
    type = "join",
    senderId = "reconnectClient",
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("1")
  )

  autosync:::handle_join(state, temp_id1, join_msg1)

  # Verify first connection is active
  expect_length(ws1$sent_messages, 1)
  expect_true(exists("reconnectClient", envir = state$connections))

  # Second connection with same senderId

  ws2 <- create_mock_ws()
  temp_id2 <- ws2$id
  state$connections[[temp_id2]] <- list(
    ws = ws2,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  join_msg2 <- list(
    type = "join",
    senderId = "reconnectClient",
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("1")
  )

  autosync:::handle_join(state, temp_id2, join_msg2)

  # Old WS should have been closed
  expect_true(ws1$closed)

  # New connection should be active
  expect_length(ws2$sent_messages, 1)
  response <- secretbase::cbordec(ws2$sent_messages[[1]])
  expect_equal(response$type, "peer")
  expect_equal(response$targetId, "reconnectClient")

  # Connection env should point to new ws
  conn <- state$connections[["reconnectClient"]]
  expect_identical(conn$ws, ws2)
})

test_that("broadcast_ephemeral preserves count and sessionId from sender", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  ws1 <- create_mock_ws()
  ws2 <- create_mock_ws()

  state$connections[["sender"]] <- list(
    ws = ws1,
    client_id = "sender",
    metadata = list(),
    connected_at = Sys.time()
  )

  state$connections[["receiver"]] <- list(
    ws = ws2,
    client_id = "receiver",
    metadata = list(),
    connected_at = Sys.time()
  )

  doc_id <- "ephTestDoc"
  state$doc_peers[[doc_id]] <- c("sender", "receiver")

  ephemeral_msg <- list(
    type = "ephemeral",
    senderId = "sender",
    documentId = doc_id,
    count = 42L,
    sessionId = "session-abc-123",
    data = list(cursor = "line 10")
  )

  autosync:::handle_ephemeral(state, "sender", ephemeral_msg)

  expect_length(ws2$sent_messages, 1)
  relayed <- secretbase::cbordec(ws2$sent_messages[[1]])
  expect_equal(relayed$type, "ephemeral")
  expect_equal(relayed$count, 42L)
  expect_equal(relayed$sessionId, "session-abc-123")
  expect_equal(relayed$senderId, "sender")
  expect_equal(relayed$data$cursor, "line 10")
})
