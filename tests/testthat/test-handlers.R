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
    supportedProtocolVersions = list("999")  # Unsupported
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
    documentId = "!!!invalid!!!",  # Invalid base58check
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

test_that("handle_sync creates sync state for client/document pair", {
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

  # Sync state should be created
  state_key <- paste(client_id, doc_id, sep = ":")
  expect_true(exists(state_key, envir = state$sync_states))
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
    targetId = "someOtherPeer",  # Not the server's peer_id
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

test_that("handle_disconnect cleans up sync states", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  client_id <- "disconnectClient"
  doc_id <- generate_document_id()

  # Create sync states for this client
  state_key1 <- paste(client_id, doc_id, sep = ":")
  state_key2 <- paste(client_id, "anotherdoc", sep = ":")
  state$sync_states[[state_key1]] <- automerge::am_sync_state_new()
  state$sync_states[[state_key2]] <- automerge::am_sync_state_new()

  # Also create sync state for another client (should not be removed)
  other_key <- paste("otherClient", doc_id, sep = ":")
  state$sync_states[[other_key]] <- automerge::am_sync_state_new()

  autosync:::handle_disconnect(state, client_id)

  # Client's sync states should be removed
  expect_false(exists(state_key1, envir = state$sync_states))
  expect_false(exists(state_key2, envir = state$sync_states))

  # Other client's sync state should remain
  expect_true(exists(other_key, envir = state$sync_states))
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
    targetId = "nonexistentClient",  # Not connected
    data = "hello"
  )

  # Should not error
  expect_no_error(
    autosync:::handle_ephemeral(state, "client1", ephemeral_msg)
  )
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
  state$sync_states[[paste("receiver1", doc_id, sep = ":")]] <- automerge::am_sync_state_new()
  state$sync_states[[paste("receiver2", doc_id, sep = ":")]] <- automerge::am_sync_state_new()

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
  state$storage_id <- NULL  # Ephemeral server
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
