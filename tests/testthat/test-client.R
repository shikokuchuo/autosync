test_that("format_hex formats raw bytes as hex", {
  format_hex <- autosync:::format_hex

  # Simple test
  bytes <- as.raw(c(0xDE, 0xAD, 0xBE, 0xEF))
  result <- format_hex(bytes)
  expect_equal(result, "de ad be ef")

  # Test with max_len limit
  bytes <- as.raw(1:10)
  result <- format_hex(bytes, max_len = 5L)
  expect_equal(result, "01 02 03 04 05")

  # Test with single byte
  bytes <- as.raw(0xFF)
  result <- format_hex(bytes)
  expect_equal(result, "ff")
})

# Test helpers shared between amsync_fetch() and amsync_client() tests

fake_peer <- secretbase::cborenc(list(
  type = "peer",
  senderId = "server123",
  selectedProtocolVersion = "1"
))

# Mock recv_aio that returns a scripted sequence of responses, one per call.
# Each element of `responses` is either an errorValue or raw CBOR bytes.
scripted_recv <- function(responses) {
  i <- 0L
  function(...) {
    i <<- i + 1L
    list(data = responses[[min(i, length(responses))]])
  }
}

# amsync_fetch() tests

test_that("amsync_fetch retrieves document from server", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  # Create and populate a document

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "test_key", "test_value")
  automerge::am_put(doc, automerge::AM_ROOT, "number", 42L)
  automerge::am_put(doc, automerge::AM_ROOT, "flag", TRUE)

  # Fetch the document
  fetched <- amsync_fetch(server$url, doc_id, timeout = 5000L, verbose = FALSE)

  expect_true(inherits(fetched, "am_doc"))
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "test_key"), "test_value")
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "number"), 42L)
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "flag"), TRUE)
})

test_that("amsync_fetch works in verbose mode", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  # Create and populate a document
  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "verbose_test", "value")

  # Fetch with verbose = TRUE and capture output
  output <- capture.output({
    fetched <- amsync_fetch(server$url, doc_id, timeout = 5000L, verbose = TRUE)
  }, type = "message")

  # Should have verbose output
  expect_true(any(grepl("\\[CLIENT\\]", output)))
  expect_true(any(grepl("Connecting", output)))
  expect_true(any(grepl("peer", output, ignore.case = TRUE)))
  expect_true(any(grepl("join", output, ignore.case = TRUE)))
  expect_true(any(grepl("sync", output, ignore.case = TRUE)))
  expect_true(any(grepl("document", output, ignore.case = TRUE)))

  # Document should still be fetched correctly
  expect_true(inherits(fetched, "am_doc"))
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "verbose_test"), "value")
})

test_that("amsync_fetch non-verbose mode produces no output", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)

  output <- capture.output({
    fetched <- amsync_fetch(server$url, doc_id, timeout = 5000L, verbose = FALSE)
  }, type = "message")

  # Should have no verbose output

  expect_length(output, 0)
})

test_that("amsync_fetch sends access_token as Authorization header", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "key", "value")

  # Server has no auth, so token is sent but ignored
  fetched <- amsync_fetch(server$url, doc_id, token = "test_token_12345_extra_padding")

  expect_true(inherits(fetched, "am_doc"))
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "key"), "value")
})

test_that("amsync_fetch errors when peer response fails", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(structure(5L, class = "errorValue"))),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_fetch("ws://fake:1234", "fake_doc_id"),
    "Failed to receive peer response"
  )
})

test_that("amsync_fetch errors when server returns error during handshake", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      secretbase::cborenc(list(type = "error", message = "auth failed"))
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_fetch("ws://fake:1234", "fake_doc_id"),
    "Server error: auth failed"
  )
})

test_that("amsync_fetch errors on unexpected message type", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      secretbase::cborenc(list(type = "sync", senderId = "server123"))
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_fetch("ws://fake:1234", "fake_doc_id"),
    "Expected peer message, got: sync"
  )
})

test_that("amsync_fetch errors when server returns error during sync", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      fake_peer,
      secretbase::cborenc(list(type = "error", message = "sync failed"))
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_fetch("ws://fake:1234", "fake_doc_id"),
    "Server error: sync failed"
  )
})

test_that("amsync_fetch errors when document is unavailable", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      fake_peer,
      secretbase::cborenc(list(type = "doc-unavailable"))
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_fetch("ws://fake:1234", "fake_doc_id"),
    "Document not available on server: fake_doc_id"
  )
})

test_that("amsync_fetch warns when no sync messages received", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      fake_peer,
      structure(5L, class = "errorValue")
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_warning(
    amsync_fetch("ws://fake:1234", "fake_doc_id"),
    "No sync messages received"
  )
})

test_that("amsync_fetch verbose mode logs CBOR decode errors", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      fake_peer,
      as.raw(c(0xFF, 0xFF, 0xFF)),
      structure(5L, class = "errorValue")
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  output <- capture.output(
    suppressWarnings(
      amsync_fetch("ws://fake:1234", "fake_doc_id", verbose = TRUE)
    ),
    type = "message"
  )
  expect_true(any(grepl("CBOR decode error", output)))
})

test_that("amsync_fetch verbose mode reports skipped foreign sync messages", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      fake_peer,
      secretbase::cborenc(list(
        type = "sync",
        documentId = "different_doc",
        data = raw(0)
      )),
      structure(5L, class = "errorValue")
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  output <- capture.output(
    suppressWarnings(
      amsync_fetch("ws://fake:1234", "fake_doc_id", verbose = TRUE)
    ),
    type = "message"
  )
  expect_true(any(grepl("Ignoring sync for different document", output)))
})

test_that("amsync_fetch verbose mode reports unknown message types", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      fake_peer,
      secretbase::cborenc(list(type = "ephemeral", data = raw(0))),
      structure(5L, class = "errorValue")
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  output <- capture.output(
    suppressWarnings(
      amsync_fetch("ws://fake:1234", "fake_doc_id", verbose = TRUE)
    ),
    type = "message"
  )
  expect_true(any(grepl("Ignoring message type: ephemeral", output)))
})

# amsync_client() tests

test_that("amsync_client errors when peer response fails", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(structure(5L, class = "errorValue"))),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_client("ws://fake:1234"),
    "Failed to receive peer response"
  )
})

test_that("amsync_client errors when server returns error during handshake", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      secretbase::cborenc(list(type = "error", message = "auth failed"))
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_client("ws://fake:1234"),
    "Server error: auth failed"
  )
})

test_that("amsync_client errors on unexpected message type during handshake", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = scripted_recv(list(
      secretbase::cborenc(list(type = "sync", senderId = "server123"))
    )),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_client("ws://fake:1234"),
    "Expected peer message, got: sync"
  )
})

test_that("open_doc errors when the server reports an error", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  conn <- amsync_client(server$url)
  on.exit(if (conn$active) conn$close(), add = TRUE)

  # An unparseable document ID makes the server send an error and disconnect.
  expect_error(
    suppressWarnings(conn$open_doc("not a valid id!")),
    "Server error"
  )
})

test_that("open_doc errors when the document is unavailable", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  # share = FALSE denies all document requests (sends doc-unavailable).
  server <- amsync_server(data_dir = data_dir, share = FALSE)
  server$start()
  on.exit(server$close(), add = TRUE)

  conn <- amsync_client(server$url)
  on.exit(if (conn$active) conn$close(), add = TRUE)

  expect_error(
    suppressWarnings(conn$open_doc(generate_document_id())),
    "Document not available on server"
  )
})

test_that("open_doc times out without a sync response", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  conn <- amsync_client(server$url)
  on.exit(if (conn$active) conn$close(), add = TRUE)

  # Drop the outgoing request so the server never replies.
  local_mocked_bindings(send_msg = function(s, msg) invisible(NULL))

  expect_error(
    conn$open_doc(generate_document_id(), timeout = 200L),
    "No sync response from server"
  )
})

test_that("amsync_client connects and open_doc receives document", {
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "greeting", "hello")

  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)

  expect_s3_class(conn, "amsync_client")
  expect_true(conn$active)

  handle <- conn$open_doc(doc_id)
  expect_s3_class(handle, "amsync_doc")
  expect_true(handle$active)
  expect_equal(handle$doc_id, doc_id)
  expect_equal(
    automerge::am_get(handle$doc, automerge::AM_ROOT, "greeting"),
    "hello"
  )
})

test_that("a document handle's $close() detaches it from the connection", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)
  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)

  handle <- conn$open_doc(doc_id)
  expect_true(handle$active)

  # Closing the handle drops the document so the handle reports inactive.
  handle$close()
  expect_false(handle$active)

  # A second close is an idempotent no-op.
  expect_no_error(handle$close())
})

test_that("open_doc errors once the connection is closed", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  conn <- amsync_client(server$url)
  conn$close()

  expect_error(
    conn$open_doc(generate_document_id()),
    "not active"
  )
})

test_that("open_doc reuses one connection across documents", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  id1 <- create_document(server)
  id2 <- create_document(server)

  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)

  conns_before <- length(ls(attr(server, "sync")$connections))
  h1 <- conn$open_doc(id1)
  h2 <- conn$open_doc(id2)
  for (i in seq_len(10)) later::run_now(0.05)

  # Opening documents reuses the same WebSocket, not a new server connection.
  expect_equal(length(ls(attr(server, "sync")$connections)), conns_before)
  expect_identical(h1$stream, conn$stream)
  expect_identical(h2$stream, conn$stream)
  expect_identical(h1$connection, conn)

  # Re-opening the same document returns a handle backed by the same live doc.
  expect_identical(conn$open_doc(id1)$doc, h1$doc)
})

test_that("open_doc handle $push() pushes local changes to server", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)

  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)
  handle <- conn$open_doc(doc_id)

  # Make a local change and push
  automerge::am_put(handle$doc, automerge::AM_ROOT, "from_client", "value1")
  handle$push()

  # Give server time to process
  for (i in seq_len(20)) later::run_now(0.1)

  server_doc <- get_document(server, doc_id)
  expect_equal(
    automerge::am_get(server_doc, automerge::AM_ROOT, "from_client"),
    "value1"
  )
})

test_that("open_doc handle receives server-side changes", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)

  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)
  handle <- conn$open_doc(doc_id)

  # Make a change on the server side and sync to client via a second client
  server_doc <- get_document(server, doc_id)
  automerge::am_put(server_doc, automerge::AM_ROOT, "server_key", "server_val")

  # Use a second fetch to trigger broadcast_sync on the server
  fetched <- amsync_fetch(server$url, doc_id, timeout = 2000L)

  # Give the async receive loop time to process
  for (i in seq_len(30)) later::run_now(0.1)

  expect_equal(
    automerge::am_get(handle$doc, automerge::AM_ROOT, "server_key"),
    "server_val"
  )
})

test_that("amsync_client deactivates when periodic sync errors", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- generate_document_id()

  conn <- amsync_client(server$url, interval = 50L)
  on.exit(if (conn$active) conn$close(), add = TRUE)
  handle <- conn$open_doc(doc_id)

  # Give the periodic sync something to send, then force send to fail
  # to trigger the timer's error handler.
  automerge::am_put(handle$doc, automerge::AM_ROOT, "k", "v")
  local_mocked_bindings(send_msg = function(s, msg) stop("simulated"))

  while (!later::loop_empty()) later::run_now(1L)

  expect_false(conn$active)
})

test_that("amsync_client async loop survives process_message errors", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- generate_document_id()

  conn <- amsync_client(server$url, interval = 999000L)
  on.exit(conn$close(), add = TRUE)
  handle <- conn$open_doc(doc_id)

  # Force every subsequent apply_sync_and_reply to throw so the async
  # receive loop has to recover.
  local_mocked_bindings(
    apply_sync_and_reply = function(...) stop("synthetic")
  )

  # Trigger a server-side broadcast to push a sync at our client
  server_doc <- get_document(server, doc_id)
  automerge::am_put(server_doc, automerge::AM_ROOT, "trigger", "v")
  expect_warning(
    amsync_fetch(server$url, doc_id, timeout = 2000L),
    "Receive error: synthetic"
  )

  # Wait for the recv to fire, snapshot, then close so the loop can drain
  # (sync_loop reschedules itself, so loop_empty() never goes true while active).
  later::run_now(2L)
  active_after_error <- conn$active
  conn$close()
  while (!later::loop_empty()) later::run_now(1L)

  expect_true(active_after_error)
})

test_that("apply_sync_and_reply warns on decode error", {
  doc <- automerge::am_create()
  sync_state <- automerge::am_sync_state()

  local_mocked_bindings(send_msg = function(s, msg) invisible(NULL))

  expect_warning(
    apply_sync_and_reply(
      s = NULL,
      doc = doc,
      sync_state = sync_state,
      data = as.raw(c(0xFF, 0xFF, 0xFF)),
      peer_id = "p",
      target_id = "t",
      doc_id = "d"
    ),
    "am_sync_decode error"
  )
})

test_that("process_message handles non-sync server messages", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  conn <- amsync_client(server$url, interval = 999000L)
  on.exit(conn$close(), add = TRUE)

  process_message <- environment(conn$open_doc)$process_message

  expect_warning(
    process_message(secretbase::cborenc(list(type = "error", message = "boom"))),
    "Server error: boom"
  )
  expect_warning(
    process_message(secretbase::cborenc(list(type = "doc-unavailable"))),
    "Document not available"
  )
  # Malformed CBOR is swallowed (process_message returns silently)
  expect_silent(process_message(as.raw(c(0xFF, 0xFF))))
})

test_that("amsync_client periodic timer is a no-op when connection becomes inactive", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  conn <- amsync_client(server$url, interval = 100L)
  on.exit(if (conn$active) conn$close(), add = TRUE)

  # Close the stream externally; the pending recv promise will reject,
  # flipping active to FALSE without cancelling the still-scheduled timer.
  # The next timer tick must hit the !active early return.
  close(conn$stream)

  deadline <- Sys.time() + 1
  while (Sys.time() < deadline) later::run_now(0.05)

  expect_false(conn$active)
})

test_that("amsync_client $close() stops the connection", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  conn <- amsync_client(server$url)
  expect_true(conn$active)

  conn$close()
  expect_false(conn$active)

  # Calling close again is a no-op
  expect_silent(conn$close())
})

test_that("print methods display connection and document info", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)

  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)

  conn_out <- capture.output(print(conn))
  expect_true(any(grepl("Automerge Sync Connection", conn_out)))
  expect_true(any(grepl("Active: TRUE", conn_out)))

  doc_out <- capture.output(print(conn$open_doc(doc_id)))
  expect_true(any(grepl("Automerge Document", doc_out)))
  expect_true(any(grepl(doc_id, doc_out, fixed = TRUE)))
  expect_true(any(grepl("Active: TRUE", doc_out)))
})

test_that("amsync_fetch returns empty document for new document ID", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()

  on.exit(server$close(), add = TRUE)

  # Request a new document ID (server creates empty doc)
  new_id <- generate_document_id()

  fetched <- amsync_fetch(server$url, new_id, timeout = 2000L, verbose = FALSE)

  # Should return a valid but empty document
  expect_true(inherits(fetched, "am_doc"))
  expect_length(automerge::am_keys(fetched, automerge::AM_ROOT), 0)
})
