# Test peer.R functions using mock objects (no network I/O)

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
  state$share <- NA
  state
}

create_mock_ws <- function() {
  ws <- new.env(hash = TRUE)
  ws$sent_messages <- list()
  ws$send <- function(data) {
    ws$sent_messages <- c(ws$sent_messages, list(data))
  }
  ws$id <- as.character(sample.int(1e6, 1))
  ws
}

set_sync_state <- function(state, client_id, doc_id, sync_state = NULL) {
  if (is.null(state$sync_states[[client_id]])) {
    state$sync_states[[client_id]] <- new.env(hash = TRUE, parent = emptyenv())
  }
  state$sync_states[[client_id]][[doc_id]] <- sync_state %||%
    automerge::am_sync_state()
}

# --- handle_peer_message tests ---

test_that("handle_peer_message processes sync message for existing document", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Create a document with data
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "key", "value")
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  # Set up remote peer connection
  remote_peer_id <- "remotePeer1"
  ws <- create_mock_ws()
  state$connections[[remote_peer_id]] <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )

  # Create a remote doc and generate sync data from it
  remote_doc <- automerge::am_create()
  remote_sync <- automerge::am_sync_state()
  sync_data <- automerge::am_sync_encode(remote_doc, remote_sync)

  msg <- list(
    type = "sync",
    senderId = remote_peer_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = sync_data
  )

  autosync:::handle_peer_message(state, remote_peer_id, msg)

  # Sync state should be created for the remote peer

  expect_true(exists(remote_peer_id, envir = state$sync_states))
  expect_true(exists(doc_id, envir = state$sync_states[[remote_peer_id]]))

  # Remote peer should be added to doc_peers
  expect_true(remote_peer_id %in% state$doc_peers[[doc_id]])

  # Should have sent a sync reply
  expect_true(length(ws$sent_messages) >= 1)
  reply <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(reply$type, "sync")
  expect_equal(reply$documentId, doc_id)
  expect_equal(reply$targetId, remote_peer_id)
})

test_that("handle_peer_message auto-creates document when enabled", {
  state <- create_test_state(auto_create_docs = TRUE)
  on.exit(unlink(state$data_dir, recursive = TRUE))

  remote_peer_id <- "remotePeer2"
  ws <- create_mock_ws()
  state$connections[[remote_peer_id]] <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )

  doc_id <- generate_document_id()

  # Create sync data from a document with content
  remote_doc <- automerge::am_create()
  automerge::am_put(remote_doc, automerge::AM_ROOT, "hello", "world")
  remote_sync <- automerge::am_sync_state()
  sync_data <- automerge::am_sync_encode(remote_doc, remote_sync)

  msg <- list(
    type = "sync",
    senderId = remote_peer_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = sync_data
  )

  autosync:::handle_peer_message(state, remote_peer_id, msg)

  # Document should be auto-created
  expect_true(exists(doc_id, envir = state$documents))
  expect_true(inherits(state$documents[[doc_id]], "am_doc"))
})

test_that("handle_peer_message skips unknown doc when auto_create_docs is FALSE", {
  state <- create_test_state(auto_create_docs = FALSE)
  on.exit(unlink(state$data_dir, recursive = TRUE))

  remote_peer_id <- "remotePeer3"
  ws <- create_mock_ws()
  state$connections[[remote_peer_id]] <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )

  doc_id <- generate_document_id()

  msg <- list(
    type = "sync",
    senderId = remote_peer_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = raw(0)
  )

  autosync:::handle_peer_message(state, remote_peer_id, msg)

  # Document should NOT be created
  expect_false(exists(doc_id, envir = state$documents))

  # No messages should be sent
  expect_length(ws$sent_messages, 0)
})

test_that("handle_peer_message ignores missing documentId", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  remote_peer_id <- "remotePeer4"
  ws <- create_mock_ws()
  state$connections[[remote_peer_id]] <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )

  msg <- list(
    type = "sync",
    senderId = remote_peer_id,
    targetId = state$peer_id
    # documentId intentionally omitted
  )

  expect_no_error(autosync:::handle_peer_message(state, remote_peer_id, msg))
  expect_length(ws$sent_messages, 0)
})

test_that("handle_peer_message ignores non-sync message types", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  remote_peer_id <- "remotePeer5"
  ws <- create_mock_ws()
  state$connections[[remote_peer_id]] <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )

  msg <- list(
    type = "ephemeral",
    senderId = remote_peer_id,
    data = "something"
  )

  expect_no_error(autosync:::handle_peer_message(state, remote_peer_id, msg))
  expect_length(ws$sent_messages, 0)
})

test_that("handle_peer_message reuses existing sync state", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  remote_peer_id <- "remotePeerExisting"
  ws <- create_mock_ws()
  state$connections[[remote_peer_id]] <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )

  # Pre-create sync state
  set_sync_state(state, remote_peer_id, doc_id)
  original_sync_env <- state$sync_states[[remote_peer_id]]

  msg <- list(
    type = "sync",
    senderId = remote_peer_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = raw(0)
  )

  autosync:::handle_peer_message(state, remote_peer_id, msg)

  # Should reuse the same sync state environment
  expect_identical(state$sync_states[[remote_peer_id]], original_sync_env)
})

test_that("handle_peer_message handles sync decode errors gracefully", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  remote_peer_id <- "remotePeerBadSync"
  ws <- create_mock_ws()
  state$connections[[remote_peer_id]] <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )

  msg <- list(
    type = "sync",
    senderId = remote_peer_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = as.raw(c(0xFF, 0xFE, 0xFD)) # Invalid sync data
  )

  # Should warn but not error
  expect_warning(
    autosync:::handle_peer_message(state, remote_peer_id, msg),
    "sync decode error"
  )
})

# --- announce_new_document tests ---

test_that("announce_new_document sends sync when share=TRUE", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))
  state$share <- TRUE

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  peer_id <- "outboundPeer"
  ws <- create_mock_ws()
  state$connections[[peer_id]] <- list(
    ws = ws,
    client_id = peer_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  autosync:::announce_new_document(state, doc_id, doc)

  expect_true(length(ws$sent_messages) >= 1)
  msg <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(msg$type, "sync")
  expect_equal(msg$documentId, doc_id)
  expect_equal(msg$targetId, peer_id)
  expect_true(peer_id %in% state$doc_peers[[doc_id]])
})

test_that("announce_new_document sends sync with share function", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  state$share <- function(peer_metadata, doc_id) {
    if (isTRUE(peer_metadata$isPeer)) TRUE else NA
  }

  peer_id <- "inboundPeer"
  ws <- create_mock_ws()
  state$connections[[peer_id]] <- list(
    ws = ws,
    client_id = peer_id,
    metadata = list(isPeer = TRUE),
    connected_at = Sys.time()
  )

  autosync:::announce_new_document(state, doc_id, doc)

  expect_true(length(ws$sent_messages) >= 1)
  msg <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(msg$type, "sync")
  expect_equal(msg$documentId, doc_id)
})

test_that("announce_new_document skips connections when share=NA", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  client_id <- "regularClient"
  ws <- create_mock_ws()
  state$connections[[client_id]] <- list(
    ws = ws,
    client_id = client_id,
    metadata = list(),
    connected_at = Sys.time()
  )

  autosync:::announce_new_document(state, doc_id, doc)

  expect_length(ws$sent_messages, 0)
})

test_that("announce_new_document skips pre-handshake connections", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  temp_id <- "tempWsId"
  ws <- create_mock_ws()
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  autosync:::announce_new_document(state, doc_id, doc)

  expect_length(ws$sent_messages, 0)
})

test_that("announce_new_document skips duplicate temp_id entries", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))
  state$share <- TRUE

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  peer_id <- "dualIndexPeer"
  temp_id <- "tempId123"
  ws <- create_mock_ws()
  conn <- list(
    ws = ws,
    client_id = peer_id,
    metadata = list(),
    connected_at = Sys.time()
  )
  # Both entries point to the same connection (dual indexing)
  state$connections[[peer_id]] <- conn
  state$connections[[temp_id]] <- conn

  autosync:::announce_new_document(state, doc_id, doc)

  # Should send exactly once (skip temp_id entry where key != client_id)
  expect_length(ws$sent_messages, 1)
})

# --- connect_peer tests ---

test_that("connect_peer warns on connection failure", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  # Use an unreachable URL to trigger connection error
  expect_warning(
    autosync:::connect_peer(state, "ws://127.0.0.1:1"),
    "Failed to connect to peer"
  )
})

test_that("handle_peer_message saves document after sync decode", {
  data_dir <- tempfile()
  state <- create_test_state(data_dir = data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  remote_peer_id <- "remotePeerSave"
  ws <- create_mock_ws()
  state$connections[[remote_peer_id]] <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )

  # Create sync data from a document with content
  remote_doc <- automerge::am_create()
  automerge::am_put(remote_doc, automerge::AM_ROOT, "saved", "yes")
  remote_sync <- automerge::am_sync_state()
  sync_data <- automerge::am_sync_encode(remote_doc, remote_sync)

  msg <- list(
    type = "sync",
    senderId = remote_peer_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = sync_data
  )

  autosync:::handle_peer_message(state, remote_peer_id, msg)

  # Document should be saved to disk
  expected_file <- file.path(data_dir, paste0(doc_id, ".automerge"))
  expect_true(file.exists(expected_file))
})

test_that("handle_peer_message with empty data does not decode", {
  state <- create_test_state()
  on.exit(unlink(state$data_dir, recursive = TRUE))

  doc <- automerge::am_create()
  doc_id <- generate_document_id()
  state$documents[[doc_id]] <- doc

  remote_peer_id <- "remotePeerEmpty"
  ws <- create_mock_ws()
  state$connections[[remote_peer_id]] <- list(
    ws = ws,
    client_id = remote_peer_id,
    metadata = list(),
    connected_at = Sys.time(),
    is_peer = TRUE
  )

  msg <- list(
    type = "sync",
    senderId = remote_peer_id,
    targetId = state$peer_id,
    documentId = doc_id,
    data = raw(0) # Empty data - length 0
  )

  # Should not warn (no decode attempt on empty data)
  expect_no_warning(
    autosync:::handle_peer_message(state, remote_peer_id, msg)
  )
})
