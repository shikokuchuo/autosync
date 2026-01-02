# Utility functions for autosync

#' Generate a peer ID
#'
#' Creates a new unique peer ID compatible with automerge-repo.
#' The ID is 16 random bytes, Base64-encoded.
#'
#' @return Character string (Base64 encoded).
#'
#' @noRd
generate_peer_id <- function() {
  secretbase::base64enc(nanonext::random(16L, convert = FALSE))
}

#' Null-coalescing operator
#'
#' @param x Value to test.
#' @param y Default value if x is NULL.
#'
#' @return x if not NULL, otherwise y.
#'
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
