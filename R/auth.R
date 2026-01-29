# Authentication module for autosync

#' Validate an OAuth2 access token
#'
#' Validates an access token by calling Google's tokeninfo endpoint.
#'
#' @param access_token Character string, the OAuth2 access token.
#' @param allowed_emails Character vector of allowed email addresses (optional).
#' @param allowed_domains Character vector of allowed email domains (optional).
#' @param custom_validator Function(token_info) returning TRUE/FALSE (optional).
#' @param token_timeout Numeric, seconds to wait for Google tokeninfo endpoint
#'   (default 5).
#'
#' @return List with `valid` (logical), `email` (character or NULL),
#'   `error` (character or NULL).
#'
#' @keywords internal
validate_token <- function(
  access_token,
  allowed_emails = NULL,
  allowed_domains = NULL,
  custom_validator = NULL,
  token_timeout = 5
) {
  url <- sprintf(
    "https://www.googleapis.com/oauth2/v3/tokeninfo?access_token=%s",
    access_token
  )

  resp <- nanonext::ncurl(url, timeout = token_timeout * 1000L)

  if (nanonext::is_error_value(resp$data)) {
    return(list(
      valid = FALSE,
      email = NULL,
      error = paste("Token validation failed:", as.character(resp$data))
    ))
  }

  token_info <- jsondec(resp$data)

  status <- resp$status

  if (status != 200L) {
    err_msg <- token_info$error_description %||%
      token_info$error %||%
      sprintf("Token validation failed (HTTP %d)", status)
    return(list(valid = FALSE, email = NULL, error = err_msg))
  }

  if (
    !is.null(token_info$expires_in) && as.integer(token_info$expires_in) <= 0L
  ) {
    return(list(valid = FALSE, email = NULL, error = "Token expired"))
  }

  email <- token_info$email

  if (!is.null(allowed_emails)) {
    if (!email %in% allowed_emails) {
      return(list(
        valid = FALSE,
        email = email,
        error = "Email not in allowlist"
      ))
    }
  }

  if (!is.null(allowed_domains)) {
    domain <- sub(".*@", "", email)
    if (!domain %in% allowed_domains) {
      return(list(valid = FALSE, email = email, error = "Domain not allowed"))
    }
  }

  if (!is.null(custom_validator)) {
    if (!isTRUE(custom_validator(token_info))) {
      return(list(
        valid = FALSE,
        email = email,
        error = "Custom validation failed"
      ))
    }
  }

  list(valid = TRUE, email = email, error = NULL)
}

#' Create an authentication configuration
#'
#' Creates a configuration object for enabling Google OAuth2 authentication
#' on an autosync server.
#'
#' @param allowed_emails Character vector of allowed email addresses.
#' @param allowed_domains Character vector of allowed email domains
#'   (e.g., "mycompany.com").
#' @param custom_validator Function(token_info) returning TRUE/FALSE for
#'   custom validation logic.
#' @param auth_timeout Numeric, seconds to wait for client to send join message
#'   with valid credentials after WebSocket connection is established.
#'   Connections that don't authenticate within this window are closed.
#'   Default 10 seconds.
#' @param token_timeout Numeric, seconds to wait for Google's tokeninfo endpoint
#'   to respond when validating tokens. Default 5 seconds.
#'
#' @return An amsync_auth_config object.
#'
#' @examples
#' # Allow specific email domains
#' auth_config(allowed_domains = c("mycompany.com", "partner.org"))
#'
#' # Allow specific users
#' auth_config(allowed_emails = c("alice@example.com", "bob@example.com"))
#'
#' # Custom validator
#' auth_config(custom_validator = function(token_info) {
#'   # Only allow tokens with at least 5 minutes remaining
#'   as.integer(token_info$expires_in) > 300
#' })
#'
#' @export
auth_config <- function(
  allowed_emails = NULL,
  allowed_domains = NULL,
  custom_validator = NULL,
  auth_timeout = 10,
  token_timeout = 5
) {
  if (
    !is.numeric(auth_timeout) || length(auth_timeout) != 1L || auth_timeout <= 0
  ) {
    stop("'auth_timeout' must be a positive number")
  }
  if (
    !is.numeric(token_timeout) ||
      length(token_timeout) != 1L ||
      token_timeout <= 0
  ) {
    stop("'token_timeout' must be a positive number")
  }

  structure(
    list(
      allowed_emails = allowed_emails,
      allowed_domains = allowed_domains,
      custom_validator = custom_validator,
      auth_timeout = auth_timeout,
      token_timeout = token_timeout
    ),
    class = "amsync_auth_config"
  )
}

#' Authenticate a client from peerMetadata
#'
#' Extracts and validates the access token from a client's peerMetadata.
#'
#' @param auth_config An amsync_auth_config object.
#' @param peer_metadata List containing client's peer metadata.
#'
#' @return List with `valid` (logical), `email` (character or NULL),
#'   `error` (character or NULL).
#'
#' @keywords internal
authenticate_client <- function(auth_config, peer_metadata) {
  if (is.null(peer_metadata) || is.null(peer_metadata$access_token)) {
    return(list(
      valid = FALSE,
      email = NULL,
      error = "Missing access_token in peerMetadata"
    ))
  }

  token <- peer_metadata$access_token

  if (!is.character(token) || length(token) != 1L) {
    return(list(
      valid = FALSE,
      email = NULL,
      error = "Invalid access_token format"
    ))
  }
  if (nchar(token) < 20L || nchar(token) > 4096L) {
    return(list(
      valid = FALSE,
      email = NULL,
      error = "Invalid access_token length"
    ))
  }
  if (grepl("[^A-Za-z0-9._~+/-]", token)) {
    return(list(
      valid = FALSE,
      email = NULL,
      error = "Invalid access_token characters"
    ))
  }

  validate_token(
    access_token = token,
    allowed_emails = auth_config$allowed_emails,
    allowed_domains = auth_config$allowed_domains,
    custom_validator = auth_config$custom_validator,
    token_timeout = auth_config$token_timeout
  )
}

#' Get an access token for autosync authentication
#'
#' Convenience wrapper around [gargle::token_fetch()] that obtains a token
#' suitable for authenticating with an autosync server.
#'
#' @param email Email address to use for authentication, or NULL for
#'   interactive selection. See [gargle::gargle_oauth_email()].
#' @param scopes OAuth2 scopes. Defaults to userinfo.email which is sufficient
#'   for identity verification.
#' @param ... Additional arguments passed to [gargle::token_fetch()].
#'
#' @return Character string containing the access token.
#'
#' @examples
#' \dontrun{
#' # Interactive authentication
#' token <- amsync_auth()
#'
#' # Use with amsync_fetch
#' doc <- amsync_fetch(
#'   url = "wss://secure-server.example.com",
#'   doc_id = "myDocId",
#'   access_token = token
#' )
#'
#' # Specify email for non-interactive use
#' token <- amsync_auth(email = "user@example.com")
#' }
#'
#' @export
amsync_auth <- function(
  email = gargle::gargle_oauth_email(),
  scopes = "https://www.googleapis.com/auth/userinfo.email",
  ...
) {
  if (!requireNamespace("gargle", quietly = TRUE)) {
    stop(
      "Package 'gargle' is required. Install with: install.packages('gargle')"
    )
  }

  token <- gargle::token_fetch(scopes = scopes, email = email, ...)

  if (is.null(token)) {
    stop("Failed to obtain OAuth2 token")
  }

  token$credentials$access_token
}

#' Check if a pending connection has timed out
#'
#' Called by later::later() to check if a client authenticated in time.
#'
#' @param server Server state environment.
#' @param temp_id Temporary connection ID.
#'
#' @keywords internal
check_auth_timeout <- function(server, temp_id) {
  if (exists(temp_id, envir = server$pending_auth, inherits = FALSE)) {
    rm(list = temp_id, envir = server$pending_auth)
    tryCatch(
      {
        send_error(server, NULL, "Authentication timeout", temp_id = temp_id)
        close_connection(server, temp_id)
      },
      error = function(e) NULL # Connection may already be closed
    )
  }
}
