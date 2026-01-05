test_that("CBOR encodes and decodes NULL", {
  encoded <- secretbase::cborenc(NULL)
  expect_equal(encoded, as.raw(0xf6))
  expect_null(secretbase::cbordec(encoded))
})

test_that("CBOR encodes and decodes booleans", {
  expect_equal(secretbase::cborenc(TRUE), as.raw(0xf5))
  expect_equal(secretbase::cborenc(FALSE), as.raw(0xf4))
  expect_true(secretbase::cbordec(as.raw(0xf5)))
  expect_false(secretbase::cbordec(as.raw(0xf4)))
})

test_that("CBOR encodes and decodes raw vectors", {
  test_raw <- as.raw(c(0xDE, 0xAD, 0xBE, 0xEF))
  encoded <- secretbase::cborenc(test_raw)
  # 0x44 = major type 2 (byte string), length 4
  expect_equal(encoded[1], as.raw(0x44))
  decoded <- secretbase::cbordec(encoded)
  expect_equal(decoded, test_raw)
})

test_that("CBOR encodes and decodes empty raw vector", {
  empty <- raw(0)
  encoded <- secretbase::cborenc(empty)
  decoded <- secretbase::cbordec(encoded)
  expect_equal(decoded, empty)
})

test_that("CBOR encodes and decodes strings", {
  test_str <- "hello world"
  encoded <- secretbase::cborenc(test_str)
  decoded <- secretbase::cbordec(encoded)
  expect_equal(decoded, test_str)
})

test_that("CBOR encodes and decodes empty string", {
  encoded <- secretbase::cborenc("")
  decoded <- secretbase::cbordec(encoded)
  expect_equal(decoded, "")
})

test_that("CBOR encodes and decodes integers", {
  # Small positive
  expect_equal(secretbase::cbordec(secretbase::cborenc(0)), 0)
  expect_equal(secretbase::cbordec(secretbase::cborenc(1)), 1)
  expect_equal(secretbase::cbordec(secretbase::cborenc(23)), 23)

  # Larger positive (1 byte additional)
  expect_equal(secretbase::cbordec(secretbase::cborenc(24)), 24)
  expect_equal(secretbase::cbordec(secretbase::cborenc(255)), 255)

  # Negative integers
  expect_equal(secretbase::cbordec(secretbase::cborenc(-1)), -1)
  expect_equal(secretbase::cbordec(secretbase::cborenc(-10)), -10)
})

test_that("CBOR encodes and decodes named lists (maps)", {
  msg <- list(
    type = "sync",
    senderId = "test123",
    count = 42L
  )
  decoded <- secretbase::cbordec(secretbase::cborenc(msg))
  expect_equal(decoded, msg)
})

test_that("CBOR encodes and decodes unnamed lists (arrays)", {
  arr <- list("1", "2", "3")
  decoded <- secretbase::cbordec(secretbase::cborenc(arr))
  expect_equal(decoded, arr)
})

test_that("CBOR roundtrips full sync message structure", {
  msg <- list(
    type = "sync",
    senderId = "dGVzdA==",
    documentId = "4NMNnk",
    data = as.raw(c(1, 2, 3, 4, 5))
  )
  decoded <- secretbase::cbordec(secretbase::cborenc(msg))
  expect_true(is.raw(decoded$data))
  expect_equal(decoded$data, msg$data)
  expect_equal(decoded, msg)
})

test_that("CBOR roundtrips nested structures (join message)", {
  join <- list(
    type = "join",
    senderId = "abc",
    peerMetadata = list(storageId = "xyz", isEphemeral = FALSE),
    supportedProtocolVersions = list("1")
  )
  decoded <- secretbase::cbordec(secretbase::cborenc(join))
  expect_equal(decoded, join)
})

test_that("CBOR roundtrips floats", {
  val <- 3.14159
  decoded <- secretbase::cbordec(secretbase::cborenc(val))
  expect_equal(decoded, val, tolerance = 1e-10)
})
