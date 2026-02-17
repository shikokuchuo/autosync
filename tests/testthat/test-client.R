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

  port <- get_test_port()
  server <- amsync_server(port = port, data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  # Create and populate a document

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "test_key", "test_value")
  automerge::am_put(doc, automerge::AM_ROOT, "number", 42L)

  # Fetch the document
  url <- paste0("ws://127.0.0.1:", port)
  fetched <- amsync_fetch(url, doc_id, timeout = 5000L, verbose = FALSE)

  expect_true(inherits(fetched, "am_doc"))
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "test_key"), "test_value")
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "number"), 42L)
})

test_that("amsync_fetch works in verbose mode", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  port <- get_test_port()
  server <- amsync_server(port = port, data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  # Create and populate a document
  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "verbose_test", "value")

  # Fetch with verbose = TRUE and capture output
  url <- paste0("ws://127.0.0.1:", port)
  output <- capture.output({
    fetched <- amsync_fetch(url, doc_id, timeout = 5000L, verbose = TRUE)
  }, type = "message")

  # Should have verbose output

  expect_true(any(grepl("\\[CLIENT\\]", output)))
  expect_true(any(grepl("Connecting", output)))
  expect_true(any(grepl("peer", output, ignore.case = TRUE)))

  # Document should still be fetched correctly
  expect_true(inherits(fetched, "am_doc"))
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "verbose_test"), "value")
})

test_that("amsync_fetch verbose mode shows sync details", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  port <- get_test_port()
  server <- amsync_server(port = port, data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "key", "value")

  url <- paste0("ws://127.0.0.1:", port)
  output <- capture.output({
    fetched <- amsync_fetch(url, doc_id, timeout = 5000L, verbose = TRUE)
  }, type = "message")

  # Check for sync-related verbose output
  expect_true(any(grepl("join", output, ignore.case = TRUE)))
  expect_true(any(grepl("sync", output, ignore.case = TRUE)))
  expect_true(any(grepl("document", output, ignore.case = TRUE)))
})

test_that("amsync_fetch non-verbose mode produces no output", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  port <- get_test_port()
  server <- amsync_server(port = port, data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)

  url <- paste0("ws://127.0.0.1:", port)
  output <- capture.output({
    fetched <- amsync_fetch(url, doc_id, timeout = 5000L, verbose = FALSE)
  }, type = "message")

  # Should have no verbose output

  expect_length(output, 0)
})

test_that("amsync_fetch handles document with multiple values", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  port <- get_test_port()
  server <- amsync_server(port = port, data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  # Create document with multiple values
  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "string", "hello")
  automerge::am_put(doc, automerge::AM_ROOT, "number", 123L)
  automerge::am_put(doc, automerge::AM_ROOT, "flag", TRUE)

  url <- paste0("ws://127.0.0.1:", port)
  fetched <- amsync_fetch(url, doc_id, timeout = 5000L, verbose = FALSE)

  # Verify all values
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "string"), "hello")
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "number"), 123L)
  expect_equal(automerge::am_get(fetched, automerge::AM_ROOT, "flag"), TRUE)
})

test_that("amsync_fetch includes access_token in peer metadata", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  port <- get_test_port()
  server <- amsync_server(port = port, data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "key", "value")

  url <- paste0("ws://127.0.0.1:", port)
  fetched <- amsync_fetch(url, doc_id, access_token = "test_token_12345")

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

test_that("amsync_fetch returns empty document for new document ID", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  port <- get_test_port()
  server <- amsync_server(port = port, data_dir = data_dir)
  server$start()

  on.exit(server$close(), add = TRUE)

  url <- paste0("ws://127.0.0.1:", port)
  # Request a new document ID (server creates empty doc)
  new_id <- generate_document_id()

  fetched <- amsync_fetch(url, new_id, timeout = 2000L, verbose = FALSE)

  # Should return a valid but empty document
  expect_true(inherits(fetched, "am_doc"))
  expect_length(automerge::am_keys(fetched, automerge::AM_ROOT), 0)
})
