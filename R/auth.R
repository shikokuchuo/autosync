# Authentication module for autosync - OIDC JWT verification

# Module-level JWKS cache keyed by issuer URL
oidc_cache <- new.env(parent = emptyenv())

#' Discover the JWKS URI for an OIDC issuer
#'
#' @param issuer The OIDC issuer URL.
#'
#' @return Character string, the JWKS URI.
#'
#' @keywords internal
discover_jwks_uri <- function(issuer) {
  config_url <- paste0(issuer, "/.well-known/openid-configuration")
  resp <- nanonext::ncurl(config_url, timeout = 5000L)

  if (nanonext::is_error_value(resp$data) || resp$status != 200L) {
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
  resp <- nanonext::ncurl(
    jwks_uri,
    timeout = 5000L,
    response = "Cache-Control"
  )

  if (nanonext::is_error_value(resp$data) || resp$status != 200L) {
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
      jose::read_jwk(jsonenc(jwk)),
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
    jsondec(jose::base64url_decode(parts[1L])),
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
    jose::jwt_decode_sig(token, key),
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
  if (is.null(claims$iss) || claims$iss != issuer) {
    return(list(valid = FALSE, email = NULL, error = "Invalid token issuer"))
  }

  # Validate audience
  aud <- claims$aud
  if (is.null(aud) || !client_id %in% aud) {
    return(list(valid = FALSE, email = NULL, error = "Invalid token audience"))
  }

  # Validate expiry
  now <- as.numeric(Sys.time())
  clock_skew <- 30
  if (is.null(claims$exp) || as.numeric(claims$exp) < now - clock_skew) {
    return(list(valid = FALSE, email = NULL, error = "Token expired"))
  }

  # Validate issued-at
  if (!is.null(claims$iat) && as.numeric(claims$iat) > now + clock_skew) {
    return(list(
      valid = FALSE,
      email = NULL,
      error = "Token issued in the future"
    ))
  }

  # Check email_verified if present
  if (isFALSE(claims$email_verified)) {
    return(list(
      valid = FALSE,
      email = claims$email,
      error = "Email not verified"
    ))
  }

  email <- claims$email

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
#' @param issuer The OIDC issuer URL. This is used to discover the provider's
#'   public keys via the `.well-known/openid-configuration` endpoint, and to
#'   validate the `iss` claim in JWTs. Defaults to Google
#'   (`"https://accounts.google.com"`).
#' @param client_id The OIDC client ID (application ID). Validated against the
#'   `aud` claim in JWTs. Defaults to the `OIDC_CLIENT_ID` environment variable.
#' @param allowed_emails Character vector of allowed email addresses.
#' @param allowed_domains Character vector of allowed email domains
#'   (e.g., "mycompany.com").
#' @param custom_validator Function(claims) returning TRUE/FALSE for
#'   custom validation logic. Receives the decoded JWT claims as a list.
#'
#' @return An amsync_auth_config object.
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
#'   issuer = "https://login.microsoftonline.com/common/v2.0",
#'   client_id = "abcdef-1234-5678",
#'   allowed_emails = "alice@mycompany.com"
#' )
#'
#' # Custom validator
#' auth_config(
#'   issuer = "https://dev-123456.okta.com/oauth2/default",
#'   client_id = "0oaXXXXXXXX",
#'   custom_validator = function(claims) "editors" %in% claims$groups
#' )
#'
#' @export
auth_config <- function(
  issuer = "https://accounts.google.com",
  client_id = Sys.getenv("OIDC_CLIENT_ID"),
  allowed_emails = NULL,
  allowed_domains = NULL,
  custom_validator = NULL
) {
  if (!is.character(issuer) || length(issuer) != 1L) {
    stop("'issuer' must be a single character string (OIDC issuer URL)")
  }

  if (!is.character(client_id) || length(client_id) != 1L || !nzchar(client_id)) {
    stop("'client_id' must be set (or set the OIDC_CLIENT_ID environment variable)")
  }

  structure(
    list(
      issuer = issuer,
      client_id = client_id,
      allowed_emails = allowed_emails,
      allowed_domains = allowed_domains,
      custom_validator = custom_validator
    ),
    class = "amsync_auth_config"
  )
}

#' Obtain an OIDC token interactively
#'
#' Performs the OAuth 2.0 Authorization Code flow with PKCE to obtain a JWT
#' (ID token) from an OIDC provider. Opens the system browser for the user to
#' authenticate, and returns the ID token for use with [amsync_fetch()].
#'
#' @param client_id The OIDC client ID (application ID). Defaults to the
#'   `OIDC_CLIENT_ID` environment variable.
#' @param client_secret The OIDC client secret. Required for "Web application"
#'   client types. Not needed for "Desktop app" client types (which use PKCE
#'   only). Defaults to the `OIDC_CLIENT_SECRET` environment variable.
#' @param issuer The OIDC issuer URL. Defaults to Google
#'   (`"https://accounts.google.com"`).
#' @param scopes Space-separated OAuth scopes to request. Default
#'   `"openid email"`.
#' @param redirect_uri Local redirect URI for the OAuth callback. Default
#'   `"http://localhost:5173"`. Must match the redirect URI registered with the
#'   OIDC provider.
#' @param timeout Seconds to wait for the user to complete authentication.
#'   Default 120.
#'
#' @return A JWT (ID token) as a character string.
#'
#' @examplesIf interactive()
#' # Uses OIDC_CLIENT_ID and OIDC_CLIENT_SECRET env vars by default
#' token <- amsync_token()
#'
#' # Or supply credentials directly
#' token <- amsync_token(
#'   client_id = "YOUR_CLIENT_ID.apps.googleusercontent.com",
#'   client_secret = "YOUR_CLIENT_SECRET"
#' )
#'
#' # Use with amsync_fetch
#' doc <- amsync_fetch(server$url, "myDocId", token = token, tls = tls)
#'
#' @export
amsync_token <- function(
  client_id = Sys.getenv("OIDC_CLIENT_ID"),
  client_secret = Sys.getenv("OIDC_CLIENT_SECRET"),
  issuer = "https://accounts.google.com",
  scopes = "openid email",
  redirect_uri = "http://localhost:5173",
  timeout = 120
) {
  if (!interactive()) {
    stop("amsync_token() requires an interactive session")
  }

  if (!is.character(client_id) || length(client_id) != 1L || !nzchar(client_id)) {
    stop("'client_id' must be set (or set the OIDC_CLIENT_ID environment variable)")
  }

  # Discover OIDC endpoints
  config_url <- paste0(issuer, "/.well-known/openid-configuration")
  resp <- nanonext::ncurl(config_url, timeout = 5000L)

  if (nanonext::is_error_value(resp$data) || resp$status != 200L) {
    stop("Failed to fetch OIDC configuration from: ", config_url)
  }

  config <- jsondec(resp$data)
  auth_endpoint <- config$authorization_endpoint
  token_endpoint <- config$token_endpoint

  if (is.null(auth_endpoint) || is.null(token_endpoint)) {
    stop(
      "OIDC configuration missing authorization_endpoint or token_endpoint"
    )
  }

  # PKCE: code_verifier is a 64-char hex string, code_challenge is its
  # base64url-encoded SHA-256 hash
  code_verifier <- random(32)
  code_challenge <- jose::base64url_encode(sha256(
    code_verifier,
    convert = FALSE
  ))

  # CSRF protection
  state <- random(16)

  # Parse redirect_uri into server URL and handler path
  parts <- nanonext::parse_url(redirect_uri)
  server_url <- paste0(parts[["scheme"]], "://", parts[["hostname"]], ":", parts[["port"]])
  handler_path <- if (nzchar(parts[["path"]])) parts[["path"]] else "/"

  # Start local callback server
  auth_result <- new.env(parent = emptyenv())

  callback_handler <- nanonext::handler(
    path = handler_path,
    callback = function(req) {
      params <- parse_query_params(req$uri)
      if (!is.null(params$error)) {
        auth_result$error <- params$error_description %||% params$error
      } else if (is.null(params$code)) {
        auth_result$error <- "No authorization code received"
      } else if (is.null(params$state) || params$state != state) {
        auth_result$error <- "State mismatch"
      } else {
        auth_result$code <- params$code
      }
      body <- if (is.null(auth_result$error)) {
        "<html><body><h2>Authentication successful</h2><p>You can close this window.</p></body></html>"
      } else {
        paste0(
          "<html><body><h2>Authentication failed</h2><p>",
          auth_result$error,
          "</p></body></html>"
        )
      }
      list(
        status = 200L,
        headers = c("Content-Type" = "text/html"),
        body = body
      )
    }
  )

  server <- nanonext::http_server(
    server_url,
    handlers = list(callback_handler)
  )
  server$start()
  on.exit(server$close())

  # Build authorization URL
  auth_url <- paste0(
    auth_endpoint,
    "?client_id=",
    utils::URLencode(client_id, reserved = TRUE),
    "&response_type=code",
    "&scope=",
    utils::URLencode(scopes, reserved = TRUE),
    "&redirect_uri=",
    utils::URLencode(redirect_uri, reserved = TRUE),
    "&state=",
    state,
    "&code_challenge=",
    code_challenge,
    "&code_challenge_method=S256"
  )

  message("Opening browser for authentication...")
  utils::browseURL(auth_url)
  message("Waiting for authentication (", timeout, "s timeout)...")

  # Wait for callback
  deadline <- Sys.time() + timeout
  while (
    is.null(auth_result$code) &&
      is.null(auth_result$error) &&
      Sys.time() < deadline
  ) {
    run_now(1L)
  }

  if (!is.null(auth_result$error)) {
    stop("Authentication failed: ", auth_result$error)
  }

  if (is.null(auth_result$code)) {
    stop("Authentication timed out after ", timeout, " seconds")
  }

  # Exchange authorization code for tokens
  token_data <- paste0(
    "code=",
    utils::URLencode(auth_result$code, reserved = TRUE),
    "&client_id=",
    utils::URLencode(client_id, reserved = TRUE),
    if (nzchar(client_secret)) paste0(
      "&client_secret=",
      utils::URLencode(client_secret, reserved = TRUE)
    ),
    "&redirect_uri=",
    utils::URLencode(redirect_uri, reserved = TRUE),
    "&grant_type=authorization_code",
    "&code_verifier=",
    utils::URLencode(code_verifier, reserved = TRUE)
  )

  token_resp <- nanonext::ncurl(
    token_endpoint,
    method = "POST",
    headers = c("Content-Type" = "application/x-www-form-urlencoded"),
    data = token_data,
    timeout = 10000L
  )

  if (nanonext::is_error_value(token_resp$data) || token_resp$status != 200L) {
    detail <- tryCatch(
      jsondec(token_resp$data)$error_description %||%
        jsondec(token_resp$data)$error,
      error = function(e) token_resp$data
    )
    stop("Token exchange failed: ", detail)
  }

  tokens <- jsondec(token_resp$data)

  if (is.null(tokens$id_token)) {
    stop("No ID token in response (ensure 'openid' scope is requested)")
  }

  tokens$id_token
}

#' Parse query parameters from a URI
#'
#' @param uri Request URI string (e.g., "/callback?code=abc&state=xyz").
#'
#' @return Named list of decoded query parameters.
#'
#' @keywords internal
parse_query_params <- function(uri) {
  query <- sub("^[^?]*\\?", "", uri)
  if (query == uri) {
    return(list())
  }
  pairs <- strsplit(query, "&", fixed = TRUE)[[1L]]
  result <- list()
  for (pair in pairs) {
    kv <- strsplit(pair, "=", fixed = TRUE)[[1L]]
    if (length(kv) == 2L) {
      result[[utils::URLdecode(kv[1L])]] <- utils::URLdecode(kv[2L])
    }
  }
  result
}

#' Authenticate a client from HTTP request headers
#'
#' Extracts and validates a Bearer token (JWT) from the Authorization header
#' of the WebSocket upgrade request.
#'
#' @param auth_config An amsync_auth_config object.
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
