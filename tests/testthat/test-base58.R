test_that("base58check roundtrip works", {
  # 16-byte document ID (same as automerge)
  bytes <- nanonext::random(16L, convert = FALSE)
  encoded <- autosync:::base58check_encode(bytes)
  decoded <- autosync:::base58check_decode(encoded)
  expect_equal(decoded, bytes)
  expect_true(nchar(encoded) %in% 27:28)
})

test_that("base58check matches automerge-repo format", {
  # Known test vector from automerge-repo tests
  known_id <- "4NMNnkMhL8jXrdJ9jamS58PAVdXu"
  known_bytes <- as.raw(c(241, 194, 156, 132, 116, 200, 74, 222,
                          184, 0, 190, 71, 98, 125, 51, 191))
  expect_equal(autosync:::base58check_decode(known_id), known_bytes)
  expect_equal(autosync:::base58check_encode(known_bytes), known_id)
})

test_that("base58check rejects invalid checksums", {
  # Valid ID with corrupted character
  invalid_id <- "4NMNnkMhL8jXrdJ9jamS58PAVdXX"  # last char changed
  expect_error(autosync:::base58check_decode(invalid_id), "checksum")
})

test_that("base58check rejects invalid characters", {
  # Base58 excludes 0, O, I, l
  expect_error(autosync:::base58check_decode("0Invalid"), "Invalid base58")
  expect_error(autosync:::base58check_decode("OInvalid"), "Invalid base58")
})

test_that("generate_document_id creates valid IDs", {
  id <- generate_document_id()
  expect_type(id, "character")
  expect_true(nchar(id) %in% 27:28)
  # Should be decodable
  expect_silent(autosync:::base58check_decode(id))
})
