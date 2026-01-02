test_that("CBOR encodes and decodes NULL", {
  encoded <- autosync:::cbor_encode(NULL)
  expect_equal(encoded, as.raw(0xf6))
  expect_null(autosync:::cbor_decode(encoded))
})

test_that("CBOR encodes and decodes booleans", {
  expect_equal(autosync:::cbor_encode(TRUE), as.raw(0xf5))
  expect_equal(autosync:::cbor_encode(FALSE), as.raw(0xf4))
  expect_true(autosync:::cbor_decode(as.raw(0xf5)))
  expect_false(autosync:::cbor_decode(as.raw(0xf4)))
})

test_that("CBOR encodes and decodes raw vectors", {
  test_raw <- as.raw(c(0xDE, 0xAD, 0xBE, 0xEF))
  encoded <- autosync:::cbor_encode(test_raw)
  # 0x44 = major type 2 (byte string), length 4
  expect_equal(encoded[1], as.raw(0x44))
  decoded <- autosync:::cbor_decode(encoded)
  expect_equal(decoded, test_raw)
})

test_that("CBOR encodes and decodes empty raw vector", {
  empty <- raw(0)
  encoded <- autosync:::cbor_encode(empty)
  decoded <- autosync:::cbor_decode(encoded)
  expect_equal(decoded, empty)
})

test_that("CBOR encodes and decodes strings", {
  test_str <- "hello world"
  encoded <- autosync:::cbor_encode(test_str)
  decoded <- autosync:::cbor_decode(encoded)
  expect_equal(decoded, test_str)
})

test_that("CBOR encodes and decodes empty string", {
  encoded <- autosync:::cbor_encode("")
  decoded <- autosync:::cbor_decode(encoded)
  expect_equal(decoded, "")
})

test_that("CBOR encodes and decodes integers", {
  # Small positive
  expect_equal(autosync:::cbor_decode(autosync:::cbor_encode(0)), 0)
  expect_equal(autosync:::cbor_decode(autosync:::cbor_encode(1)), 1)
  expect_equal(autosync:::cbor_decode(autosync:::cbor_encode(23)), 23)

  # Larger positive (1 byte additional)
  expect_equal(autosync:::cbor_decode(autosync:::cbor_encode(24)), 24)
  expect_equal(autosync:::cbor_decode(autosync:::cbor_encode(255)), 255)

  # Negative integers
  expect_equal(autosync:::cbor_decode(autosync:::cbor_encode(-1)), -1)
  expect_equal(autosync:::cbor_decode(autosync:::cbor_encode(-10)), -10)
})

test_that("CBOR encodes and decodes named lists (maps)", {
  msg <- list(
    type = "sync",
    senderId = "test123",
    count = 42L
  )
  decoded <- autosync:::cbor_decode(autosync:::cbor_encode(msg))
  expect_equal(decoded, msg)
})

test_that("CBOR encodes and decodes unnamed lists (arrays)", {
  arr <- list("1", "2", "3")
  decoded <- autosync:::cbor_decode(autosync:::cbor_encode(arr))
  expect_equal(decoded, arr)
})

test_that("CBOR roundtrips full sync message structure", {
  msg <- list(
    type = "sync",
    senderId = "dGVzdA==",
    documentId = "4NMNnk",
    data = as.raw(c(1, 2, 3, 4, 5))
  )
  decoded <- autosync:::cbor_decode(autosync:::cbor_encode(msg))
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
  decoded <- autosync:::cbor_decode(autosync:::cbor_encode(join))
  expect_equal(decoded, join)
})

test_that("CBOR roundtrips floats", {
  val <- 3.14159
  decoded <- autosync:::cbor_decode(autosync:::cbor_encode(val))
  expect_equal(decoded, val, tolerance = 1e-10)
})
