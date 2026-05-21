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
    recv_aio = function(...) list(data = structure(5L, class = "errorValue")),
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_fetch("ws://fake:1234", "fake_doc_id"),
    "Failed to receive peer response"
  )
})

test_that("amsync_fetch errors on unexpected message type", {
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = function(...) {
      fake_msg <- secretbase::cborenc(
        list(type = "sync", senderId = "server123")
      )
      list(data = fake_msg)
    },
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_fetch("ws://fake:1234", "fake_doc_id"),
    "Expected peer message, got: sync"
  )
})

test_that("amsync_fetch warns when no sync messages received", {
  recv_count <- 0L
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = function(...) {
      recv_count <<- recv_count + 1L
      if (recv_count == 1L) {
        fake_peer <- secretbase::cborenc(list(
          type = "peer",
          senderId = "server123",
          selectedProtocolVersion = "1"
        ))
        list(data = fake_peer)
      } else {
        list(data = structure(5L, class = "errorValue"))
      }
    },
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_warning(
    amsync_fetch("ws://fake:1234", "fake_doc_id"),
    "No sync messages received"
  )
})

# amsync_client() tests

test_that("amsync_client errors when no sync response is received", {
  recv_count <- 0L
  local_mocked_bindings(
    stream = function(...) rawConnection(raw(0)),
    send = function(...) invisible(NULL),
    recv_aio = function(...) {
      recv_count <<- recv_count + 1L
      if (recv_count == 1L) {
        fake_peer <- secretbase::cborenc(list(
          type = "peer",
          senderId = "server123",
          selectedProtocolVersion = "1"
        ))
        list(data = fake_peer)
      } else {
        list(data = structure(5L, class = "errorValue"))
      }
    },
    unresolved = function(...) FALSE,
    run_now = function(...) invisible(NULL)
  )

  expect_error(
    amsync_client("ws://fake:1234", "fake_doc_id", timeout = 100L),
    "No sync response from server"
  )
})

test_that("amsync_client connects and receives document", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "greeting", "hello")

  client <- amsync_client(server$url, doc_id)
  on.exit(client$close(), add = TRUE)

  expect_s3_class(client, "amsync_client")
  expect_true(client$active)
  expect_equal(
    automerge::am_get(client$doc, automerge::AM_ROOT, "greeting"),
    "hello"
  )
})

test_that("amsync_client $push() pushes local changes to server", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)

  client <- amsync_client(server$url, doc_id)
  on.exit(client$close(), add = TRUE)

  # Make a local change and push
  automerge::am_put(client$doc, automerge::AM_ROOT, "from_client", "value1")
  client$push()

  # Give server time to process
  for (i in seq_len(20)) later::run_now(0.1)

  server_doc <- get_document(server, doc_id)
  expect_equal(
    automerge::am_get(server_doc, automerge::AM_ROOT, "from_client"),
    "value1"
  )
})

test_that("amsync_client receives server-side changes", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)

  client <- amsync_client(server$url, doc_id)
  on.exit(client$close(), add = TRUE)

  # Make a change on the server side and sync to client via a second client
  server_doc <- get_document(server, doc_id)
  automerge::am_put(server_doc, automerge::AM_ROOT, "server_key", "server_val")

  # Use a second fetch to trigger broadcast_sync on the server
  fetched <- amsync_fetch(server$url, doc_id, timeout = 2000L)

  # Give the async receive loop time to process
  for (i in seq_len(30)) later::run_now(0.1)

  expect_equal(
    automerge::am_get(client$doc, automerge::AM_ROOT, "server_key"),
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

  client <- amsync_client(server$url, doc_id, interval = 50L)
  on.exit(if (client$active) client$close(), add = TRUE)

  # Give the periodic sync something to send, then force send to fail
  # to trigger the timer's error handler.
  automerge::am_put(client$doc, automerge::AM_ROOT, "k", "v")
  local_mocked_bindings(send_msg = function(s, msg) stop("simulated"))

  while (!later::loop_empty()) later::run_now(1L)

  expect_false(client$active)
})

test_that("amsync_client async loop survives process_message errors", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- generate_document_id()

  client <- amsync_client(server$url, doc_id, interval = 999000L)
  on.exit(client$close(), add = TRUE)

  # Force every subsequent apply_sync_and_reply to throw so the async
  # receive loop has to recover.
  local_mocked_bindings(
    apply_sync_and_reply = function(...) stop("synthetic")
  )

  # Trigger a server-side broadcast to push a sync at our client
  server_doc <- get_document(server, doc_id)
  automerge::am_put(server_doc, automerge::AM_ROOT, "trigger", "v")
  amsync_fetch(server$url, doc_id, timeout = 2000L)

  # Wait for the recv to fire, snapshot, then close so the loop can drain
  # (sync_loop reschedules itself, so loop_empty() never goes true while active).
  later::run_now(2L)
  active_after_error <- client$active
  client$close()
  while (!later::loop_empty()) later::run_now(1L)

  expect_true(active_after_error)
})

test_that("amsync_client $close() stops the client", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)

  client <- amsync_client(server$url, doc_id)
  expect_true(client$active)

  client$close()
  expect_false(client$active)

  # Calling close again is a no-op
  expect_silent(client$close())
})

test_that("print.amsync_client displays info", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)

  client <- amsync_client(server$url, doc_id)
  on.exit(client$close(), add = TRUE)

  output <- capture.output(print(client))
  expect_true(any(grepl("Automerge Sync Client", output)))
  expect_true(any(grepl(doc_id, output, fixed = TRUE)))
  expect_true(any(grepl("Active: TRUE", output)))
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
