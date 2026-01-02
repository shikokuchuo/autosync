# Pure R CBOR encoder/decoder for Automerge sync protocol
#
# CBOR Major Types:
# 0 = unsigned integer    4 = array
# 1 = negative integer    5 = map
# 2 = byte string         7 = simple values (false, true, null) and floats
# 3 = text string
#
# This implementation supports the subset of RFC 8949 required for automerge-repo
# message encoding. RCBOR was evaluated but does not support raw vectors.

#' Encode length with CBOR major type header
#'
#' @param major_type Integer 0-7 indicating CBOR major type.
#' @param length Non-negative integer length value.
#'
#' @return Raw vector containing the header bytes.
#'
#' @noRd
cbor_encode_head <- function(major_type, length) {
  if (length <= 23L) {
    as.raw(major_type * 32L + length)
  } else if (length <= 255L) {
    c(as.raw(major_type * 32L + 24L), as.raw(length))
  } else if (length <= 65535L) {
    c(as.raw(major_type * 32L + 25L),
      as.raw(length %/% 256L), as.raw(length %% 256L))
  } else if (length <= 4294967295) {
    c(as.raw(major_type * 32L + 26L),
      as.raw((length %/% 16777216L) %% 256L),
      as.raw((length %/% 65536L) %% 256L),
      as.raw((length %/% 256L) %% 256L),
      as.raw(length %% 256L))
  } else {
    stop("Value too large for CBOR encoding")
  }
}

#' Encode R value to CBOR
#'
#' Encodes an R value to CBOR binary format. Supports NULL, logical, raw,
#' character, numeric, and list types.
#'
#' @param x R value to encode.
#'
#' @return Raw vector of CBOR bytes.
#'
#' @noRd
cbor_encode <- function(x) {
  if (is.null(x)) {
    as.raw(0xf6L)  # null
  } else if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    if (x) as.raw(0xf5L) else as.raw(0xf4L)  # true/false
  } else if (is.raw(x)) {
    c(cbor_encode_head(2L, length(x)), x)  # byte string
  } else if (is.character(x) && length(x) == 1L) {
    bytes <- charToRaw(x)
    c(cbor_encode_head(3L, length(bytes)), bytes)  # text string
  } else if (is.numeric(x) && length(x) == 1L) {
    if (x == floor(x) && x >= 0 && x <= 4294967295) {
      cbor_encode_head(0L, as.integer(x))  # unsigned int
    } else if (x == floor(x) && x < 0 && x >= -4294967296) {
      cbor_encode_head(1L, as.integer(-1L - x))  # negative int
    } else {
      # Float64 - must manually handle big-endian byte order
      bytes <- writeBin(x, raw(), size = 8L)
      if (.Platform$endian == "little") bytes <- rev(bytes)
      c(as.raw(0xfbL), bytes)  # float64
    }
  } else if (is.list(x)) {
    if (!is.null(names(x)) && length(names(x)) > 0L && all(names(x) != "")) {
      # Named list = map
      n <- length(x)
      result <- cbor_encode_head(5L, n)
      for (i in seq_along(x)) {
        result <- c(result, cbor_encode(names(x)[i]), cbor_encode(x[[i]]))
      }
      result
    } else {
      # Unnamed list = array
      n <- length(x)
      result <- cbor_encode_head(4L, n)
      for (item in x) {
        result <- c(result, cbor_encode(item))
      }
      result
    }
  } else {
    stop("Unsupported type for CBOR encoding: ", class(x)[1L])
  }
}

#' Decode CBOR to R value
#'
#' Decodes CBOR binary data to an R value.
#'
#' @param raw_data Raw vector of CBOR bytes.
#'
#' @return Decoded R value.
#'
#' @noRd
cbor_decode <- function(raw_data) {
  env <- new.env(parent = emptyenv())
  env$pos <- 1L
  env$data <- raw_data
  env$len <- length(raw_data)

  read_bytes <- function(n) {
    if (env$pos + n - 1L > env$len) stop("Unexpected end of CBOR data")
    result <- env$data[env$pos:(env$pos + n - 1L)]
    env$pos <- env$pos + n
    result
  }

  read_head <- function() {
    b <- as.integer(read_bytes(1L))
    major <- b %/% 32L
    additional <- b %% 32L

    # For major type 7 (simple values and floats), don't read extra bytes here
    # The float bytes are read in decode_value
    len <- if (major == 7L) {
      additional  # Just return additional for type 7
    } else if (additional <= 23L) {
      additional
    } else if (additional == 24L) {
      as.integer(read_bytes(1L))
    } else if (additional == 25L) {
      bytes <- read_bytes(2L)
      as.integer(bytes[1L]) * 256L + as.integer(bytes[2L])
    } else if (additional == 26L) {
      bytes <- read_bytes(4L)
      as.integer(bytes[1L]) * 16777216L + as.integer(bytes[2L]) * 65536L +
        as.integer(bytes[3L]) * 256L + as.integer(bytes[4L])
    } else if (additional == 27L) {
      bytes <- read_bytes(8L)
      # For 64-bit integers, we need to handle potential overflow
      sum(as.numeric(bytes) * c(72057594037927936, 281474976710656,
                                 1099511627776, 4294967296,
                                 16777216, 65536, 256, 1))
    } else {
      additional
    }
    list(major = major, additional = additional, len = len)
  }

  decode_value <- function() {
    head <- read_head()

    if (head$major == 0L) {
      head$len  # unsigned integer
    } else if (head$major == 1L) {
      -1L - head$len  # negative integer
    } else if (head$major == 2L) {
      if (head$len == 0L) raw(0L) else read_bytes(head$len)  # byte string
    } else if (head$major == 3L) {
      if (head$len == 0L) "" else rawToChar(read_bytes(head$len))  # text string
    } else if (head$major == 4L) {
      # array
      if (head$len == 0L) {
        list()
      } else {
        result <- vector("list", head$len)
        for (i in seq_len(head$len)) result[[i]] <- decode_value()
        result
      }
    } else if (head$major == 5L) {
      # map
      if (head$len == 0L) {
        structure(list(), names = character(0L))
      } else {
        result <- vector("list", head$len)
        nms <- character(head$len)
        for (i in seq_len(head$len)) {
          nms[i] <- decode_value()
          result[[i]] <- decode_value()
        }
        names(result) <- nms
        result
      }
    } else if (head$major == 7L) {
      if (head$additional == 20L) {
        FALSE
      } else if (head$additional == 21L) {
        TRUE
      } else if (head$additional == 22L) {
        NULL
      } else if (head$additional == 23L) {
        NA  # undefined -> NA in R
      } else if (head$additional == 25L) {
        # Float16 - read 2 bytes (not commonly used but included for completeness)
        bytes <- read_bytes(2L)
        # Simple implementation - may need refinement for edge cases
        stop("Float16 not fully supported")
      } else if (head$additional == 26L) {
        # Float32 - read 4 bytes
        bytes <- read_bytes(4L)
        if (.Platform$endian == "little") bytes <- rev(bytes)
        readBin(bytes, "double", 1L, 4L)
      } else if (head$additional == 27L) {
        # Float64 - read 8 bytes
        bytes <- read_bytes(8L)
        if (.Platform$endian == "little") bytes <- rev(bytes)
        readBin(bytes, "double", 1L, 8L)
      } else {
        stop("Unsupported CBOR simple value: ", head$additional)
      }
    } else {
      stop("Unsupported CBOR major type: ", head$major)
    }
  }

  decode_value()
}
