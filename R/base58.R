# Base58check encoding for Automerge document IDs
# Uses gmp for big integer arithmetic and secretbase for SHA256 checksums
#
# Automerge uses Base58check encoding WITHOUT a version byte (unlike Bitcoin).
# Document IDs are 16-byte UUIDs that encode to ~27-28 character strings.

# Bitcoin Base58 alphabet (excludes 0, O, I, l to avoid ambiguity)
.base58_alphabet <- "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

#' Convert raw bytes to Base58 string
#'
#' @param bytes Raw vector to encode.
#'
#' @return Character string in Base58.
#'
#' @noRd
bytes_to_base58 <- function(bytes) {
  # Count leading zero bytes (map to '1' in base58)
  leading_zeros <- 0L
  for (b in bytes) {
    if (b == 0L) leading_zeros <- leading_zeros + 1L
    else break
  }

  # Convert bytes to big integer using hex string with 0x prefix
  hex_chars <- sprintf("%02x", as.integer(bytes))
  hex_str <- paste0("0x", paste0(hex_chars, collapse = ""))
  num <- gmp::as.bigz(hex_str)

  # Convert to base58
  result <- character(0L)
  fifty_eight <- gmp::as.bigz(58L)
  zero <- gmp::as.bigz(0L)

  while (num > zero) {
    rem <- num %% fifty_eight
    remainder <- as.integer(as.character(rem))
    result <- c(substr(.base58_alphabet, remainder + 1L, remainder + 1L), result)
    num <- num %/% fifty_eight
  }

  # Add leading '1's for leading zero bytes
  paste(c(rep("1", leading_zeros), result), collapse = "")
}

#' Convert Base58 string to raw bytes
#'
#' @param str Base58 encoded string.
#'
#' @return Raw vector.
#'
#' @noRd
base58_to_bytes <- function(str) {
  # Count leading '1's (represent zero bytes)
  leading_ones <- 0L
  chars <- strsplit(str, "")[[1L]]
  for (char in chars) {
    if (char == "1") leading_ones <- leading_ones + 1L
    else break
  }

  # Convert from base58 to big integer
  num <- gmp::as.bigz(0L)
  fifty_eight <- gmp::as.bigz(58L)
  for (char in chars) {
    val <- regexpr(char, .base58_alphabet, fixed = TRUE) - 1L
    if (val < 0L) stop("Invalid base58 character: ", char)
    num <- num * fifty_eight + val
  }

  # Convert to bytes
  result <- raw(0L)
  two_fifty_six <- gmp::as.bigz(256L)
  zero <- gmp::as.bigz(0L)

  while (num > zero) {
    rem <- num %% two_fifty_six
    result <- c(as.raw(as.integer(as.character(rem))), result)
    num <- num %/% two_fifty_six
  }

  c(raw(leading_ones), result)
}

#' Encode bytes to Base58check (automerge variant - no version byte)
#'
#' @param bytes Raw vector to encode (16 bytes for document IDs).
#'
#' @return Character string (~27-28 chars for 16-byte input).
#'
#' @noRd
base58check_encode <- function(bytes) {
  # Calculate checksum (first 4 bytes of double SHA256)
  hash1 <- secretbase::sha256(bytes, convert = FALSE)
  hash2 <- secretbase::sha256(hash1, convert = FALSE)
  checksum <- hash2[1L:4L]

  # Append checksum and encode
  payload <- c(bytes, checksum)
  bytes_to_base58(payload)
}

#' Decode Base58check to bytes (automerge variant - no version byte)
#'
#' @param str Base58check encoded string.
#'
#' @return Raw vector.
#'
#' @noRd
base58check_decode <- function(str) {
  # Decode from Base58
  payload <- base58_to_bytes(str)

  # Split: data + checksum (4 bytes)
  n <- length(payload)
  if (n < 5L) stop("Invalid Base58check: too short")

  data <- payload[1L:(n - 4L)]
  checksum <- payload[(n - 3L):n]

  # Verify checksum
  hash1 <- secretbase::sha256(data, convert = FALSE)
  hash2 <- secretbase::sha256(hash1, convert = FALSE)
  expected <- hash2[1L:4L]

  if (!identical(checksum, expected)) {
    stop("Invalid Base58check: checksum mismatch")
  }

  data
}

#' Generate a new document ID
#'
#' Creates a new unique document ID compatible with automerge-repo.
#' The ID is a 16-byte random value encoded with Base58check.
#'
#' @return Character string (Base58check encoded, ~27-28 characters).
#'
#' @export
generate_document_id <- function() {
  bytes <- nanonext::random(16L, convert = FALSE)
  base58check_encode(bytes)
}
