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

test_that("str.am_doc handles empty document", {
  doc <- automerge::am_create()
  output <- capture.output(str(doc))
  expect_true(any(grepl("(empty)", output, fixed = TRUE)))
})

test_that("str.am_doc handles document with string value", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "name", "test value")

  output <- capture.output(str(doc))
  expect_true(any(grepl("name", output)))
  expect_true(any(grepl("test value", output)))
})

test_that("str.am_doc handles document with long string", {
  doc <- automerge::am_create()
  long_string <- paste(rep("x", 100), collapse = "")
  automerge::am_put(doc, automerge::AM_ROOT, "long", long_string)

  output <- capture.output(str(doc))
  expect_true(any(grepl("long", output)))
  expect_true(any(grepl("\\.\\.\\.", output)))  # Should be truncated
})

test_that("str.am_doc handles document with numeric value", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "count", 42L)

  output <- capture.output(str(doc))
  expect_true(any(grepl("count", output)))
  expect_true(any(grepl("42", output)))
})

test_that("str.am_doc handles document with boolean values", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "enabled", TRUE)

  output <- capture.output(str(doc))
  expect_true(any(grepl("enabled", output)))
  expect_true(any(grepl("true", output)))
})

test_that("str.am_doc handles false boolean value", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "disabled", FALSE)

  output <- capture.output(str(doc))
  expect_true(any(grepl("disabled", output)))
  expect_true(any(grepl("false", output)))
})

test_that("str.am_doc handles max.level parameter", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "key", "value")

  # With max.level = 0, should still show top level
  output <- capture.output(str(doc, max.level = 0))
  expect_true(any(grepl("key", output)))
})

test_that("str.am_doc handles unknown value types", {
  doc <- automerge::am_create()
  # Raw bytes are stored but may come back as a different type
  automerge::am_put(doc, automerge::AM_ROOT, "data", as.raw(1:5))

  output <- capture.output(str(doc))
  expect_true(any(grepl("data", output)))
})

test_that("str.am_doc handles nested objects", {
  data <- list(
    nested = list(
      inner_key = "inner_value",
      inner_num = 123
    )
  )
  doc <- automerge::as_automerge(data)

  output <- capture.output(str(doc))
  expect_true(any(grepl("nested.*\\{object\\}", output)))
  expect_true(any(grepl("inner_key", output)))
  expect_true(any(grepl("inner_value", output)))
  expect_true(any(grepl("inner_num", output)))
  expect_true(any(grepl("123", output)))
})

test_that("str.am_doc handles lists", {
  data <- list(
    items = list("first", "second", "third")
  )
  doc <- automerge::as_automerge(data)

  output <- capture.output(str(doc))
  expect_true(any(grepl("items.*\\[list, length 3\\]", output)))
  expect_true(any(grepl("\\[1\\].*first", output)))
  expect_true(any(grepl("\\[2\\].*second", output)))
  expect_true(any(grepl("\\[3\\].*third", output)))
})

test_that("str.am_doc truncates lists with more than 5 items", {
  data <- list(
    many = list("a", "b", "c", "d", "e", "f", "g", "h")
  )
  doc <- automerge::as_automerge(data)

  output <- capture.output(str(doc))
  expect_true(any(grepl("\\[list, length 8\\]", output)))
  expect_true(any(grepl("\\[5\\].*e", output)))
  expect_false(any(grepl("\\[6\\]", output)))
  expect_true(any(grepl("\\.\\.\\. and 3 more items", output)))
})

test_that("str.am_doc handles lists containing objects", {
  data <- list(
    objects = list(
      list(name = "first"),
      list(name = "second")
    )
  )
  doc <- automerge::as_automerge(data)

  output <- capture.output(str(doc))
  expect_true(any(grepl("objects.*\\[list, length 2\\]", output)))
  expect_true(any(grepl("\\[1\\].*\\{object\\}", output)))
  expect_true(any(grepl("\\[2\\].*\\{object\\}", output)))
})

test_that("str.am_doc respects max.level for deep nesting", {
  data <- list(
    level1 = list(
      level2 = list(
        level3 = list(
          value = "deep"
        )
      )
    )
  )
  doc <- automerge::as_automerge(data)

  # max.level = 0 shows only top level

  output0 <- capture.output(str(doc, max.level = 0))
  expect_true(any(grepl("level1.*\\{object\\}", output0)))
  expect_false(any(grepl("level2", output0)))

  # max.level = 1 shows one level of nesting
  output1 <- capture.output(str(doc, max.level = 1))
  expect_true(any(grepl("level2.*\\{object\\}", output1)))
  expect_false(any(grepl("level3", output1)))

  # max.level = 3 shows all levels
  output3 <- capture.output(str(doc, max.level = 3))
  expect_true(any(grepl("value.*deep", output3)))
})

test_that("str.am_doc handles multiple top-level keys", {
  data <- list(
    alpha = "a",
    beta = "b",
    gamma = "c"
  )
  doc <- automerge::as_automerge(data)

  output <- capture.output(str(doc))
  expect_true(any(grepl("alpha", output)))
  expect_true(any(grepl("beta", output)))
  expect_true(any(grepl("gamma", output)))
})

test_that("str.am_doc handles mixed value types", {
  data <- list(
    string_val = "hello",
    num_val = 42,
    bool_val = TRUE,
    nested = list(key = "value"),
    items = list(1, 2, 3)
  )
  doc <- automerge::as_automerge(data)

  output <- capture.output(str(doc))
  expect_true(any(grepl("string_val.*hello", output)))
  expect_true(any(grepl("num_val.*42", output)))
  expect_true(any(grepl("bool_val.*true", output)))
  expect_true(any(grepl("nested.*\\{object\\}", output)))
  expect_true(any(grepl("items.*\\[list", output)))
})

test_that("str.am_doc truncates long strings in list items", {
  long_string <- paste(rep("x", 100), collapse = "")
  data <- list(
    items = list(long_string)
  )
  doc <- automerge::as_automerge(data)

  output <- capture.output(str(doc))
  # List items are truncated at 40 chars
  expect_true(any(grepl("\\.\\.\\.", output)))
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
