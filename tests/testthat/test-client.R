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

test_that("print_doc_structure handles empty document", {
  doc <- automerge::am_create()
  output <- capture.output(autosync:::print_doc_structure(doc))
  expect_true(any(grepl("(empty)", output, fixed = TRUE)))
})

test_that("print_doc_structure handles document with string value", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "name", "test value")

  output <- capture.output(autosync:::print_doc_structure(doc))
  expect_true(any(grepl("name", output)))
  expect_true(any(grepl("test value", output)))
})

test_that("print_doc_structure handles document with long string", {
  doc <- automerge::am_create()
  long_string <- paste(rep("x", 100), collapse = "")
  automerge::am_put(doc, automerge::AM_ROOT, "long", long_string)

  output <- capture.output(autosync:::print_doc_structure(doc))
  expect_true(any(grepl("long", output)))
  expect_true(any(grepl("\\.\\.\\.", output)))  # Should be truncated
})

test_that("print_doc_structure handles document with numeric value", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "count", 42L)

  output <- capture.output(autosync:::print_doc_structure(doc))
  expect_true(any(grepl("count", output)))
  expect_true(any(grepl("42", output)))
})

test_that("print_doc_structure handles document with boolean values", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "enabled", TRUE)

  output <- capture.output(autosync:::print_doc_structure(doc))
  expect_true(any(grepl("enabled", output)))
  expect_true(any(grepl("true", output)))
})

test_that("print_doc_structure handles false boolean value", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "disabled", FALSE)

  output <- capture.output(autosync:::print_doc_structure(doc))
  expect_true(any(grepl("disabled", output)))
  expect_true(any(grepl("false", output)))
})

test_that("print_doc_structure handles max_depth parameter", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "key", "value")

  # With max_depth = 0, should still show top level
  output <- capture.output(
    autosync:::print_doc_structure(doc, max_depth = 0)
  )
  expect_true(any(grepl("key", output)))
})

test_that("print_doc_structure handles prefix parameter", {
  doc <- automerge::am_create()
  automerge::am_put(doc, automerge::AM_ROOT, "key", "value")

  output <- capture.output(
    autosync:::print_doc_structure(doc, prefix = ">>")
  )
  expect_true(any(grepl(">>key", output)))
})

test_that("print_doc_structure handles unknown value types", {
  doc <- automerge::am_create()
  # Raw bytes are stored but may come back as a different type
  automerge::am_put(doc, automerge::AM_ROOT, "data", as.raw(1:5))

  output <- capture.output(autosync:::print_doc_structure(doc))
  expect_true(any(grepl("data", output)))
})
