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
    conn$ws$close()
  }
}
