# Utility functions for autosync

#' Generate a new document ID
#'
#' Creates a new unique document ID compatible with automerge-repo.
#' The ID is a 16-byte random value encoded with Base58Check.
#'
#' @return Character string (Base58Check encoded).
#'
#' @export
generate_document_id <- function() base58enc(random(16L, convert = FALSE))

#' Generate a peer ID
#'
#' Creates a new unique peer ID compatible with automerge-repo.
#' The ID is 16 random bytes, Base64-encoded.
#'
#' @return Character string (Base64 encoded).
#'
#' @noRd
generate_peer_id <- function() base64enc(random(16L, convert = FALSE))

#' Null-coalescing operator
#'
#' @param x Value to test.
#' @param y Default value if x is NULL.
#'
#' @return x if not NULL, otherwise y.
#'
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Parse JSON string to list
#'
#' Minimal JSON parser for simple flat objects using base R.
#' Handles the Google tokeninfo response format.
#'
#' @param x Character string or raw vector containing JSON.
#'
#' @return Parsed list.
#'
#' @noRd
parse_json <- function(x) {
  if (is.raw(x)) {
    x <- rawToChar(x)
  }
  # Remove outer braces and whitespace

  x <- trimws(x)
  if (!startsWith(x, "{") || !endsWith(x, "}")) {
    return(list())
  }
  x <- substr(x, 2L, nchar(x) - 1L)

  # Split on commas not inside quotes
  parts <- strsplit(x, ',(?=(?:[^"]*"[^"]*")*[^"]*$)', perl = TRUE)[[1L]]

  result <- list()
  for (part in parts) {
    # Split key:value
    kv <- strsplit(trimws(part), '\\s*:\\s*', perl = TRUE)[[1L]]
    if (length(kv) >= 2L) {
      key <- gsub('^"|"$', "", trimws(kv[1L]))
      val <- trimws(paste(kv[-1L], collapse = ":"))
      # Remove quotes from string values
      if (startsWith(val, '"') && endsWith(val, '"')) {
        val <- substr(val, 2L, nchar(val) - 1L)
      }
      result[[key]] <- val
    }
  }
  result
}

#' Close a WebSocket connection
#'
#' Closes a WebSocket connection and cleans up pending auth state.
#'
#' @param server Server state environment.
#' @param id Connection ID (temp_id or client_id).
#'
#' @keywords internal
close_connection <- function(server, id) {
  # Clean up pending_auth if exists
  if (exists(id, envir = server$pending_auth, inherits = FALSE)) {
    rm(list = id, envir = server$pending_auth)
  }

  conn <- server$connections[[id]]
  if (!is.null(conn) && !is.null(conn$ws)) {
    tryCatch(
      close(conn$ws),
      error = function(e) NULL
    )
  }
}
