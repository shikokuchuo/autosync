# Authentication module for autosync - OIDC JWT verification

# Module-level JWKS cache keyed by issuer URL
oidc_cache <- new.env(parent = emptyenv())

# Wrapper for interactive() to allow mocking in tests
is_interactive <- function() interactive()

#' Default OIDC issuer URL
#'
#' Returns the `OIDC_ISSUER` environment variable if set and non-empty,
#' otherwise falls back to Google (`"https://accounts.google.com"`).
#'
#' @return Character string, the OIDC issuer URL.
#'
#' @keywords internal
oidc_issuer <- function() {
  issuer <- Sys.getenv("OIDC_ISSUER")
  if (nzchar(issuer)) issuer else "https://accounts.google.com"
}

#' Discover the JWKS URI for an OIDC issuer
#'
#' @param issuer The OIDC issuer URL.
#'
#' @return Character string, the JWKS URI.
#'
#' @keywords internal
discover_jwks_uri <- function(issuer) {
  config_url <- paste0(issuer, "/.well-known/openid-configuration")
  resp <- ncurl(config_url, timeout = 5000L)

  if (is_error_value(resp$data) || resp$status != 200L) {
    stop("Failed to fetch OIDC configuration from: ", config_url)
  }

  config <- jsondec(resp$data)
  jwks_uri <- config$jwks_uri

  if (is.null(jwks_uri)) {
    stop("No jwks_uri found in OIDC configuration from: ", config_url)
  }

  jwks_uri
}

#' Fetch and parse a JWKS endpoint
#'
#' @param jwks_uri URL of the JWKS endpoint.
#'
#' @return List with `keys` (named list of jose key objects by kid) and
#'   `expiry` (POSIXct when the cache should be refreshed).
#'
#' @keywords internal
fetch_jwks <- function(jwks_uri) {
  resp <- ncurl(
    jwks_uri,
    timeout = 5000L,
    response = "Cache-Control"
  )

  if (is_error_value(resp$data) || resp$status != 200L) {
    stop("Failed to fetch JWKS from: ", jwks_uri)
  }

  jwks <- jsondec(resp$data)

  if (is.null(jwks$keys) || !length(jwks$keys)) {
    stop("No keys found in JWKS from: ", jwks_uri)
  }

  keys <- list()
  for (jwk in jwks$keys) {
    kid <- jwk$kid
    if (is.null(kid)) {
      next
    }
    key <- tryCatch(
      read_jwk(jsonenc(jwk)),
      error = function(e) NULL
    )
    if (!is.null(key)) {
      keys[[kid]] <- key
    }
  }

  # Parse Cache-Control max-age for TTL (default 1 hour)
  ttl <- 3600L
  cache_control <- resp$headers[["Cache-Control"]]
  if (!is.null(cache_control)) {
    m <- regmatches(cache_control, regexpr("max-age=(\\d+)", cache_control))
    if (length(m) && nzchar(m)) {
      ttl <- as.integer(sub("max-age=", "", m))
    }
  }

  list(keys = keys, expiry = Sys.time() + ttl)
}

#' Get a signing key for JWT verification
#'
#' Looks up the public key for the given issuer and key ID, refreshing the
#' cache if needed.
#'
#' @param issuer The OIDC issuer URL.
#' @param kid The key ID from the JWT header.
#'
#' @return A public key object, or NULL if not found.
#'
#' @keywords internal
get_signing_key <- function(issuer, kid) {
  cache_key <- issuer

  cached <- oidc_cache[[cache_key]]

  # Return cached key if available and not expired
  if (!is.null(cached) && Sys.time() < cached$expiry) {
    key <- cached$keys[[kid]]
    if (!is.null(key)) return(key)
  }

  # Refresh: discover JWKS URI (use cached URI if available)
  jwks_uri <- if (!is.null(cached$jwks_uri)) {
    cached$jwks_uri
  } else {
    discover_jwks_uri(issuer)
  }

  jwks_data <- fetch_jwks(jwks_uri)
  oidc_cache[[cache_key]] <- list(
    jwks_uri = jwks_uri,
    keys = jwks_data$keys,
    expiry = jwks_data$expiry
  )

  jwks_data$keys[[kid]]
}

#' Validate a JWT token
#'
#' Verifies a JWT locally using the issuer's public keys (JWKS) and validates
#' standard OIDC claims.
#'
#' @param token Character string, the JWT.
#' @param issuer Expected issuer URL.
#' @param client_id Expected audience (client ID).
#' @param allowed_emails Character vector of allowed email addresses (optional).
#' @param allowed_domains Character vector of allowed email domains (optional).
#' @param custom_validator Function(claims) returning TRUE/FALSE (optional).
#'
#' @return List with `valid` (logical), `email` (character or NULL),
#'   `error` (character or NULL).
#'
#' @keywords internal
validate_token <- function(
  token,
  issuer,
  client_id,
  allowed_emails = NULL,
  allowed_domains = NULL,
  custom_validator = NULL
) {
  # Extract kid from JWT header
  parts <- strsplit(token, ".", fixed = TRUE)[[1L]]
  if (length(parts) != 3L) {
    return(list(valid = FALSE, email = NULL, error = "Invalid JWT format"))
  }

  header <- tryCatch(
    jsondec(base64dec(parts[1L], url = TRUE)),
    error = function(e) NULL
  )
  if (is.null(header) || is.null(header$kid)) {
    return(list(valid = FALSE, email = NULL, error = "Invalid JWT header"))
  }

  # Get the signing key
  key <- tryCatch(
    get_signing_key(issuer, header$kid),
    error = function(e) NULL
  )
  if (is.null(key)) {
    return(list(
      valid = FALSE,
      email = NULL,
      error = "Unable to verify token signature"
    ))
  }

  # Verify signature and decode claims
  claims <- tryCatch(
    jwt_decode_sig(token, key),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("expired", msg, ignore.case = TRUE)) {
        structure(msg, class = "token_expired")
      } else {
        NULL
      }
    }
  )

  if (is.null(claims)) {
    return(list(
      valid = FALSE,
      email = NULL,
      error = "Token signature verification failed"
    ))
  }

  if (inherits(claims, "token_expired")) {
    return(list(valid = FALSE, email = NULL, error = "Token expired"))
  }

  # Validate issuer
  if (is.null(claims[["iss"]]) || claims[["iss"]] != issuer) {
    return(list(valid = FALSE, email = NULL, error = "Invalid token issuer"))
  }

  # Validate audience
  aud <- claims[["aud"]]
  if (is.null(aud) || !client_id %in% aud) {
    return(list(valid = FALSE, email = NULL, error = "Invalid token audience"))
  }

  # Validate expiry
  now <- as.numeric(Sys.time())
  clock_skew <- 30
  if (
    is.null(claims[["exp"]]) || as.numeric(claims[["exp"]]) < now - clock_skew
  ) {
    return(list(valid = FALSE, email = NULL, error = "Token expired"))
  }

  # Validate issued-at
  if (
    !is.null(claims[["iat"]]) && as.numeric(claims[["iat"]]) > now + clock_skew
  ) {
    return(list(
      valid = FALSE,
      email = NULL,
      error = "Token issued in the future"
    ))
  }

  email <- claims[["email"]]

  # Check email_verified if present
  if (isFALSE(claims[["email_verified"]])) {
    return(list(
      valid = FALSE,
      email = email,
      error = "Email not verified"
    ))
  }

  # An allowlist requires a present email and email_verified == TRUE.
  if (!is.null(allowed_emails) || !is.null(allowed_domains)) {
    if (is.null(email) || !nzchar(email)) {
      return(list(
        valid = FALSE,
        email = NULL,
        error = "Token has no email claim"
      ))
    }
    if (!isTRUE(claims[["email_verified"]])) {
      return(list(
        valid = FALSE,
        email = email,
        error = "Email not verified"
      ))
    }
  }

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
    if (!isTRUE(custom_validator(claims))) {
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
#' Creates a configuration object for enabling OIDC JWT authentication
#' on an autosync server. When enabled, clients must include a valid JWT
#' (ID token) as a Bearer token in the Authorization header of the WebSocket
#' upgrade request. Connections without valid credentials are rejected
#' immediately at connection time.
#'
#' Works with any OIDC-compliant identity provider: Google, Microsoft Entra,
#' Okta, Auth0, etc.
#'
#' @param client_id The OIDC client ID (application ID). Validated against the
#'   `aud` claim in JWTs. Defaults to the `OIDC_CLIENT_ID` environment variable.
#' @param issuer The OIDC issuer URL. This is used to discover the provider's
#'   public keys via the `.well-known/openid-configuration` endpoint, and to
#'   validate the `iss` claim in JWTs. Defaults to the `OIDC_ISSUER`
#'   environment variable, falling back to Google
#'   (`"https://accounts.google.com"`).
#' @param allowed_emails Character vector of allowed email addresses. When set,
#'   a token is rejected unless it carries an `email` claim with
#'   `email_verified` explicitly `TRUE`.
#' @param allowed_domains Character vector of allowed email domains
#'   (e.g., "mycompany.com"). Subject to the same verified-email requirement as
#'   `allowed_emails`.
#' @param custom_validator Function(claims) returning TRUE/FALSE for
#'   custom validation logic. Receives the decoded JWT claims as a list.
#'
#' @return An object of class `"sync_auth_config"`.
#'
#' @examples
#' # Google (default issuer)
#' auth_config(
#'   client_id = "123456789.apps.googleusercontent.com",
#'   allowed_domains = "mycompany.com"
#' )
#'
#' # Microsoft Entra
#' auth_config(
#'   client_id = "abcdef-1234-5678",
#'   issuer = "https://login.microsoftonline.com/common/v2.0",
#'   allowed_emails = "alice@mycompany.com"
#' )
#'
#' # Custom validator
#' auth_config(
#'   client_id = "0oaXXXXXXXX",
#'   issuer = "https://dev-123456.okta.com/oauth2/default",
#'   custom_validator = function(claims) "editors" %in% claims$groups
#' )
#'
#' @export
auth_config <- function(
  client_id = Sys.getenv("OIDC_CLIENT_ID"),
  issuer = oidc_issuer(),
  allowed_emails = NULL,
  allowed_domains = NULL,
  custom_validator = NULL
) {
  if (!is.character(issuer) || length(issuer) != 1L) {
    stop("'issuer' must be a single character string (OIDC issuer URL)")
  }

  if (
    !is.character(client_id) || length(client_id) != 1L || !nzchar(client_id)
  ) {
    stop(
      "'client_id' must be set (or set the OIDC_CLIENT_ID environment variable)"
    )
  }

  structure(
    list(
      issuer = issuer,
      client_id = client_id,
      allowed_emails = allowed_emails,
      allowed_domains = allowed_domains,
      custom_validator = custom_validator
    ),
    class = "sync_auth_config"
  )
}

#' Obtain an OIDC token interactively
#'
#' Performs the OAuth 2.0 Authorization Code flow with PKCE to obtain a JWT
#' (ID token) from an OIDC provider, delegating the browser handshake and token
#' exchange to \pkg{httr2}. Endpoints are discovered from the issuer's
#' `.well-known` metadata via [httr2::oauth_server_metadata()], and the flow is
#' run by [httr2::oauth_flow_auth_code()]: it opens the system browser, listens
#' on a loopback redirect for the callback, and returns the ID token for use
#' with [sync_fetch()].
#'
#' For Google, register the OAuth client as a "Desktop app" and set both
#' `OIDC_CLIENT_ID` and `OIDC_CLIENT_SECRET`. Google's Desktop app secret is
#' required in the token exchange but, unlike a "Web application" secret, is
#' not treated as confidential: Google states that for installed apps "the
#' client secret is obviously not treated as a secret"
#' (<https://developers.google.com/identity/protocols/oauth2#installed>),
#' consistent with the OAuth 2.0 for Native Apps standard
#' (RFC 8252 section 8.5,
#' <https://datatracker.ietf.org/doc/html/rfc8252#section-8.5>).
#' Providers that support native / public clients (Microsoft Entra, Okta,
#' Auth0, etc.) need only `client_id`, authenticating via PKCE alone; leave
#' `client_secret` unset for these.
#'
#' @param client_id The OIDC client ID (application ID). Defaults to the
#'   `OIDC_CLIENT_ID` environment variable.
#' @param client_secret The OIDC client secret. Required by Google (Desktop
#'   app) and "Web application" client types; leave unset for native / public
#'   clients, which authenticate via PKCE alone. Defaults to the
#'   `OIDC_CLIENT_SECRET` environment variable.
#' @param issuer The OIDC issuer URL. Defaults to the `OIDC_ISSUER`
#'   environment variable, falling back to Google
#'   (`"https://accounts.google.com"`).
#' @param scopes Space-separated OAuth scopes to request. Default
#'   `"openid email"`.
#' @param redirect_uri Local redirect URI for the OAuth callback. Defaults to
#'   [httr2::oauth_redirect_uri()], i.e. `"http://localhost"` with an
#'   OS-assigned random port (or the `HTTR2_OAUTH_REDIRECT_URL` environment
#'   variable on hosted platforms). Supply an explicit port (e.g.
#'   `"http://localhost:8080"`) when your OIDC provider requires the redirect
#'   URI to match a pre-registered value.
#'
#' @return A JWT (ID token) as a character string.
#'
#' @examplesIf interactive()
#' # Uses OIDC_CLIENT_ID and OIDC_CLIENT_SECRET env vars by default
#' token <- sync_token()
#'
#' # Or supply credentials directly
#' token <- sync_token(
#'   client_id = "YOUR_CLIENT_ID.apps.googleusercontent.com",
#'   client_secret = "YOUR_CLIENT_SECRET"
#' )
#'
#' # Use with sync_fetch
#' doc <- sync_fetch(server$url, "myDocId", token = token, tls = tls)
#'
#' @export
sync_token <- function(
  client_id = Sys.getenv("OIDC_CLIENT_ID"),
  client_secret = Sys.getenv("OIDC_CLIENT_SECRET"),
  issuer = oidc_issuer(),
  scopes = "openid email",
  redirect_uri = oauth_redirect_uri()
) {
  if (!is_interactive()) {
    stop("sync_token() requires an interactive session")
  }

  if (
    !is.character(client_id) || length(client_id) != 1L || !nzchar(client_id)
  ) {
    stop(
      "'client_id' must be set (or set the OIDC_CLIENT_ID environment variable)"
    )
  }

  # Discover the provider's OAuth/OIDC endpoints from its well-known metadata.
  metadata <- oauth_server_metadata(issuer)
  if (
    is.null(metadata$authorization_endpoint) ||
      is.null(metadata$token_endpoint)
  ) {
    stop("OIDC configuration missing authorization_endpoint or token_endpoint")
  }

  # An empty secret means a public / native client: authenticate via PKCE alone.
  client <- oauth_client(
    id = client_id,
    secret = if (nzchar(client_secret)) client_secret else NULL,
    metadata = metadata
  )

  # Authorization Code + PKCE: opens the browser, listens on the loopback
  # redirect for the callback, then exchanges the code for tokens.
  token <- oauth_flow_auth_code(
    client,
    scope = scopes,
    redirect_uri = redirect_uri
  )

  if (is.null(token$id_token)) {
    stop("No ID token in response (ensure 'openid' scope is requested)")
  }

  token$id_token
}

#' Authenticate a client from HTTP request headers
#'
#' Extracts and validates a Bearer token (JWT) from the Authorization header
#' of the WebSocket upgrade request.
#'
#' @param auth_config A `sync_auth_config` object.
#' @param headers Named list of HTTP request headers.
#'
#' @return List with `valid` (logical), `email` (character or NULL),
#'   `error` (character or NULL).
#'
#' @keywords internal
authenticate_header <- function(auth_config, headers) {
  auth_fail <- list(
    valid = FALSE,
    email = NULL,
    error = "Authentication failed"
  )

  if (
    is.null(headers) || !length(headers) || !"Authorization" %in% names(headers)
  ) {
    return(auth_fail)
  }

  auth_header <- unname(headers["Authorization"])
  if (!startsWith(auth_header, "Bearer ")) {
    return(auth_fail)
  }

  token <- substring(auth_header, 8L)

  # JWT format: Base64URL chars and dots, typically 800-2000 chars
  if (
    nchar(token) < 20L ||
      nchar(token) > 8192L ||
      grepl("[^A-Za-z0-9_.-]", token)
  ) {
    return(auth_fail)
  }

  validate_token(
    token = token,
    issuer = auth_config$issuer,
    client_id = auth_config$client_id,
    allowed_emails = auth_config$allowed_emails,
    allowed_domains = auth_config$allowed_domains,
    custom_validator = auth_config$custom_validator
  )
}
