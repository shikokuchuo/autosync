# Tests for OIDC JWT authentication module

# Helper: create a test JWT signed with a given RSA key
create_test_jwt <- function(claims, key, kid = "test-kid-1") {
  claim <- do.call(jose::jwt_claim, claims)
  jose::jwt_encode_sig(claim, key, header = list(kid = kid))
}

# Helper: standard valid claims
valid_claims <- function(
  issuer = "https://accounts.google.com",
  client_id = "test-client-id",
  email = "user@test.com"
) {
  now <- as.integer(Sys.time())
  list(
    iss = issuer,
    aud = client_id,
    sub = "1234567890",
    email = email,
    email_verified = TRUE,
    iat = now - 60L,
    exp = now + 3600L
  )
}

# ---- auth_config tests ----

test_that("auth_config creates valid configuration", {
  cfg <- auth_config(
    issuer = "https://accounts.google.com",
    client_id = "test-client-id",
    allowed_domains = "example.com",
    allowed_emails = "test@example.com"
  )

  expect_s3_class(cfg, "amsync_auth_config")
  expect_equal(cfg$issuer, "https://accounts.google.com")
  expect_equal(cfg$client_id, "test-client-id")
  expect_equal(cfg$allowed_domains, "example.com")
  expect_equal(cfg$allowed_emails, "test@example.com")
})

test_that("oidc_issuer reads OIDC_ISSUER env var", {
  old <- Sys.getenv("OIDC_ISSUER")
  on.exit(if (nzchar(old)) Sys.setenv(OIDC_ISSUER = old) else Sys.unsetenv("OIDC_ISSUER"))
  Sys.setenv(OIDC_ISSUER = "https://login.microsoftonline.com/common/v2.0")
  expect_equal(oidc_issuer(), "https://login.microsoftonline.com/common/v2.0")
})

test_that("oidc_issuer falls back to Google when OIDC_ISSUER is unset", {
  old <- Sys.getenv("OIDC_ISSUER")
  on.exit(if (nzchar(old)) Sys.setenv(OIDC_ISSUER = old) else Sys.unsetenv("OIDC_ISSUER"))
  Sys.unsetenv("OIDC_ISSUER")
  expect_equal(oidc_issuer(), "https://accounts.google.com")
})

test_that("oidc_issuer falls back to Google when OIDC_ISSUER is empty", {
  old <- Sys.getenv("OIDC_ISSUER")
  on.exit(if (nzchar(old)) Sys.setenv(OIDC_ISSUER = old) else Sys.unsetenv("OIDC_ISSUER"))
  Sys.setenv(OIDC_ISSUER = "")
  expect_equal(oidc_issuer(), "https://accounts.google.com")
})

test_that("auth_config defaults issuer from OIDC_ISSUER env var", {
  old <- Sys.getenv("OIDC_ISSUER")
  on.exit(if (nzchar(old)) Sys.setenv(OIDC_ISSUER = old) else Sys.unsetenv("OIDC_ISSUER"))
  Sys.setenv(OIDC_ISSUER = "https://login.microsoftonline.com/common/v2.0")
  cfg <- auth_config(client_id = "test-id")
  expect_equal(cfg$issuer, "https://login.microsoftonline.com/common/v2.0")
})

test_that("auth_config validates issuer and client_id", {
  expect_snapshot(auth_config(issuer = 123, client_id = "x"), error = TRUE)
  expect_snapshot(auth_config(issuer = "x", client_id = 123), error = TRUE)
  expect_snapshot(auth_config(client_id = ""), error = TRUE)
})

test_that("auth_config has NULL defaults for optional parameters", {
  cfg <- auth_config(
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )
  expect_null(cfg$allowed_emails)
  expect_null(cfg$allowed_domains)
  expect_null(cfg$custom_validator)
})

test_that("auth_config stores custom_validator function", {
  validator <- function(claims) "editors" %in% claims$groups
  cfg <- auth_config(
    issuer = "https://accounts.google.com",
    client_id = "test-id",
    custom_validator = validator
  )
  expect_identical(cfg$custom_validator, validator)
})

# ---- authenticate_header tests ----

test_that("authenticate_header returns error for NULL headers", {
  cfg <- auth_config(
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )
  result <- authenticate_header(cfg, NULL)
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")
})

test_that("authenticate_header returns generic error for invalid credentials", {
  cfg <- auth_config(
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )

  # Missing headers
  result <- authenticate_header(cfg, c())
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Missing Authorization header
  result <- authenticate_header(cfg, c(Host = "localhost"))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Wrong scheme (not Bearer)
  result <- authenticate_header(cfg, c(Authorization = "Basic dXNlcjpwYXNz"))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Too short token
  result <- authenticate_header(cfg, c(Authorization = "Bearer short"))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Too long (over 8192 chars)
  result <- authenticate_header(
    cfg,
    c(Authorization = paste("Bearer", strrep("a", 9000)))
  )
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Contains special chars invalid for JWT
  result <- authenticate_header(
    cfg,
    c(Authorization = paste0("Bearer ", strrep("a", 25), "$%^&", strrep("b", 25)))
  )
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Empty token after Bearer
  result <- authenticate_header(cfg, c(Authorization = "Bearer "))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")
})

test_that("authenticate_header accepts token at minimum length boundary", {
  cfg <- auth_config(
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )
  local_mocked_bindings(
    validate_token = function(...) list(valid = TRUE, email = "a@b.com", error = NULL)
  )
  result <- authenticate_header(cfg, c(Authorization = paste("Bearer", strrep("a", 20))))
  expect_true(result$valid)
})

test_that("authenticate_header accepts token at maximum length boundary", {
  cfg <- auth_config(
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )
  local_mocked_bindings(
    validate_token = function(...) list(valid = TRUE, email = "a@b.com", error = NULL)
  )
  result <- authenticate_header(cfg, c(Authorization = paste("Bearer", strrep("a", 8192))))
  expect_true(result$valid)
})

test_that("authenticate_header accepts valid JWT characters", {
  cfg <- auth_config(
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )
  local_mocked_bindings(
    validate_token = function(...) list(valid = TRUE, email = "a@b.com", error = NULL)
  )
  # All allowed characters: A-Za-z0-9_.-
  token <- "ABCDEFghij0123456789_.-"
  result <- authenticate_header(cfg, c(Authorization = paste("Bearer", token)))
  expect_true(result$valid)
})

test_that("authenticate_header passes config to validate_token", {
  cfg <- auth_config(
    issuer = "https://accounts.google.com",
    client_id = "test-client-id",
    allowed_emails = c("user@test.com"),
    allowed_domains = c("test.com"),
    custom_validator = function(x) TRUE
  )

  captured_args <- NULL
  local_mocked_bindings(
    validate_token = function(...) {
      captured_args <<- list(...)
      list(valid = TRUE, email = "user@test.com", error = NULL)
    }
  )

  token <- strrep("x", 30)
  authenticate_header(cfg, c(Authorization = paste("Bearer", token)))

  expect_equal(captured_args$token, token)
  expect_equal(captured_args$issuer, "https://accounts.google.com")
  expect_equal(captured_args$client_id, "test-client-id")
  expect_equal(captured_args$allowed_emails, c("user@test.com"))
  expect_equal(captured_args$allowed_domains, c("test.com"))
  expect_true(is.function(captured_args$custom_validator))
})

# ---- validate_token JWT tests ----

test_that("validate_token verifies a valid JWT", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims()

  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id"
  )

  expect_true(result$valid)
  expect_equal(result$email, "user@test.com")
  expect_null(result$error)
})

test_that("validate_token rejects invalid JWT format", {
  result <- validate_token(
    token = "not-a-jwt",
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Invalid JWT format")
})

test_that("validate_token rejects JWT with bad header", {
  result <- validate_token(
    token = "!!!.bbb.ccc",
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Invalid JWT header")
})

test_that("validate_token rejects JWT with unknown kid", {
  key <- openssl::rsa_keygen(2048)
  claims <- valid_claims()
  jwt <- create_test_jwt(claims, key, kid = "unknown-kid")

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) NULL
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Unable to verify token signature")
})

test_that("validate_token rejects JWT signed with wrong key", {
  signing_key <- openssl::rsa_keygen(2048)
  wrong_key <- openssl::rsa_keygen(2048)
  wrong_pubkey <- as.list(wrong_key)$pubkey
  claims <- valid_claims()

  jwt <- create_test_jwt(claims, signing_key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) wrong_pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Token signature verification failed")
})

test_that("validate_token rejects wrong issuer", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims(issuer = "https://evil.com")
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Invalid token issuer")
})

test_that("validate_token rejects wrong audience", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims(client_id = "wrong-client-id")
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Invalid token audience")
})

test_that("validate_token rejects expired JWT", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims()
  claims$exp <- as.integer(Sys.time()) - 120L  # expired 2 minutes ago
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Token expired")
})

test_that("validate_token rejects JWT issued in the future", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims()
  claims$iat <- as.integer(Sys.time()) + 120L  # 2 minutes in future
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Token issued in the future")
})

test_that("validate_token rejects unverified email", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims()
  claims$email_verified <- FALSE
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Email not verified")
})

test_that("validate_token rejects email not in allowlist", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims(email = "stranger@other.com")
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id",
    allowed_emails = c("alice@test.com", "bob@test.com")
  )
  expect_false(result$valid)
  expect_equal(result$email, "stranger@other.com")
  expect_equal(result$error, "Email not in allowlist")
})

test_that("validate_token accepts email in allowlist", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims(email = "alice@test.com")
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id",
    allowed_emails = c("alice@test.com", "bob@test.com")
  )
  expect_true(result$valid)
  expect_equal(result$email, "alice@test.com")
})

test_that("validate_token rejects disallowed domain", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims(email = "user@evil.com")
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id",
    allowed_domains = c("trusted.com", "partner.org")
  )
  expect_false(result$valid)
  expect_equal(result$email, "user@evil.com")
  expect_equal(result$error, "Domain not allowed")
})

test_that("validate_token accepts allowed domain", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims(email = "user@trusted.com")
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id",
    allowed_domains = c("trusted.com", "partner.org")
  )
  expect_true(result$valid)
  expect_equal(result$email, "user@trusted.com")
})

test_that("validate_token rejects when custom_validator returns FALSE", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims()
  claims$groups <- "viewers"
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id",
    custom_validator = function(claims) "editors" %in% claims$groups
  )
  expect_false(result$valid)
  expect_equal(result$email, "user@test.com")
  expect_equal(result$error, "Custom validation failed")
})

test_that("validate_token accepts when custom_validator returns TRUE", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims()
  claims$groups <- "editors"
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id",
    custom_validator = function(claims) "editors" %in% claims$groups
  )
  expect_true(result$valid)
  expect_equal(result$email, "user@test.com")
})

test_that("validate_token applies email, domain, and custom checks in order", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims(email = "user@trusted.com")
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id",
    allowed_emails = c("user@trusted.com"),
    allowed_domains = c("trusted.com"),
    custom_validator = function(claims) TRUE
  )
  expect_true(result$valid)
  expect_equal(result$email, "user@trusted.com")
})

# ---- OIDC discovery and JWKS cache tests ----

test_that("discover_jwks_uri fetches from well-known endpoint", {
  local_mocked_bindings(
    ncurl = function(url, ...) {
      expect_match(url, "\\.well-known/openid-configuration$")
      list(
        data = charToRaw('{"jwks_uri":"https://www.googleapis.com/oauth2/v3/certs"}'),
        status = 200L
      )
    },
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  uri <- discover_jwks_uri("https://accounts.google.com")
  expect_equal(uri, "https://www.googleapis.com/oauth2/v3/certs")
})

test_that("discover_jwks_uri errors on failed request", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = structure(5L, class = "errorValue"),
      status = 0L
    ),
    is_error_value = function(x) inherits(x, "errorValue"),
    .package = "nanonext"
  )

  expect_error(
    discover_jwks_uri("https://bad-issuer.com"),
    "Failed to fetch OIDC configuration"
  )
})

test_that("discover_jwks_uri errors on non-200 status", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw("Not Found"),
      status = 404L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  expect_error(
    discover_jwks_uri("https://bad-issuer.com"),
    "Failed to fetch OIDC configuration"
  )
})

test_that("discover_jwks_uri errors on missing jwks_uri", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"issuer":"https://accounts.google.com"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  expect_error(
    discover_jwks_uri("https://accounts.google.com"),
    "No jwks_uri found"
  )
})

test_that("get_signing_key caches and refreshes keys", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  jwk_json <- jose::write_jwk(pubkey)
  jwk <- jsondec(jwk_json)
  jwk$kid <- "cached-kid"

  # Clear cache
  rm(list = ls(oidc_cache), envir = oidc_cache)

  fetch_count <- 0L
  local_mocked_bindings(
    discover_jwks_uri = function(issuer) "https://example.com/jwks",
    fetch_jwks = function(jwks_uri) {
      fetch_count <<- fetch_count + 1L
      list(
        keys = list("cached-kid" = pubkey),
        expiry = Sys.time() + 3600
      )
    }
  )

  # First call should fetch
  result <- get_signing_key("https://example.com", "cached-kid")
  expect_false(is.null(result))
  expect_equal(fetch_count, 1L)

  # Second call should use cache
  result2 <- get_signing_key("https://example.com", "cached-kid")
  expect_false(is.null(result2))
  expect_equal(fetch_count, 1L)

  # Clean up
  rm(list = ls(oidc_cache), envir = oidc_cache)
})

test_that("get_signing_key refreshes on unknown kid", {
  key1 <- openssl::rsa_keygen(2048)
  pubkey1 <- as.list(key1)$pubkey
  key2 <- openssl::rsa_keygen(2048)
  pubkey2 <- as.list(key2)$pubkey

  # Clear cache
  rm(list = ls(oidc_cache), envir = oidc_cache)

  call_count <- 0L
  local_mocked_bindings(
    discover_jwks_uri = function(issuer) "https://example.com/jwks",
    fetch_jwks = function(jwks_uri) {
      call_count <<- call_count + 1L
      keys <- list("kid-1" = pubkey1)
      if (call_count >= 2L) keys[["kid-2"]] <- pubkey2
      list(keys = keys, expiry = Sys.time() + 3600)
    }
  )

  # First fetch gets kid-1
  result <- get_signing_key("https://example.com", "kid-1")
  expect_false(is.null(result))
  expect_equal(call_count, 1L)

  # Unknown kid-2 triggers refresh
  result2 <- get_signing_key("https://example.com", "kid-2")
  expect_false(is.null(result2))
  expect_equal(call_count, 2L)

  # Clean up
  rm(list = ls(oidc_cache), envir = oidc_cache)
})

test_that("get_signing_key returns NULL for unknown kid after refresh", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey

  # Clear cache
  rm(list = ls(oidc_cache), envir = oidc_cache)

  local_mocked_bindings(
    discover_jwks_uri = function(issuer) "https://example.com/jwks",
    fetch_jwks = function(jwks_uri) {
      list(
        keys = list("kid-1" = pubkey),
        expiry = Sys.time() + 3600
      )
    }
  )

  # Request a kid that doesn't exist even after fetch
  result <- get_signing_key("https://example.com", "nonexistent-kid")
  expect_null(result)

  # Clean up
  rm(list = ls(oidc_cache), envir = oidc_cache)
})

test_that("get_signing_key refreshes expired cache", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey

  # Clear cache
  rm(list = ls(oidc_cache), envir = oidc_cache)

  # Seed cache with expired entry
  oidc_cache[["https://example.com"]] <- list(
    jwks_uri = "https://example.com/jwks",
    keys = list("kid-1" = pubkey),
    expiry = Sys.time() - 1  # already expired
  )

  fetch_count <- 0L
  local_mocked_bindings(
    fetch_jwks = function(jwks_uri) {
      fetch_count <<- fetch_count + 1L
      list(keys = list("kid-1" = pubkey), expiry = Sys.time() + 3600)
    }
  )

  result <- get_signing_key("https://example.com", "kid-1")
  expect_false(is.null(result))
  expect_equal(fetch_count, 1L)

  # Clean up
  rm(list = ls(oidc_cache), envir = oidc_cache)
})

# ---- fetch_jwks tests ----

test_that("fetch_jwks parses keys and cache-control", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  jwk <- jsondec(jose::write_jwk(pubkey))
  jwk$kid <- "test-kid"
  jwks_json <- jsonenc(list(keys = list(jwk)))

  local_mocked_bindings(
    ncurl = function(url, ...) list(
      data = charToRaw(jwks_json),
      status = 200L,
      headers = list("Cache-Control" = "public, max-age=7200")
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- fetch_jwks("https://example.com/jwks")
  expect_true("test-kid" %in% names(result$keys))
  expect_true(result$expiry > Sys.time() + 7000)
})

test_that("fetch_jwks uses default TTL without cache-control", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  jwk <- jsondec(jose::write_jwk(pubkey))
  jwk$kid <- "test-kid"
  jwks_json <- jsonenc(list(keys = list(jwk)))

  local_mocked_bindings(
    ncurl = function(url, ...) list(
      data = charToRaw(jwks_json),
      status = 200L,
      headers = list()
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- fetch_jwks("https://example.com/jwks")
  expect_true("test-kid" %in% names(result$keys))
  expect_true(result$expiry > Sys.time() + 3500)
  expect_true(result$expiry < Sys.time() + 3700)
})

test_that("fetch_jwks errors on failed request", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = structure(5L, class = "errorValue"),
      status = 0L
    ),
    is_error_value = function(x) inherits(x, "errorValue"),
    .package = "nanonext"
  )

  expect_error(fetch_jwks("https://example.com/jwks"), "Failed to fetch JWKS")
})

test_that("fetch_jwks errors on non-200 status", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw("Server Error"),
      status = 500L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  expect_error(fetch_jwks("https://example.com/jwks"), "Failed to fetch JWKS")
})

test_that("fetch_jwks errors on empty keys", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"keys":[]}'),
      status = 200L,
      headers = list()
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  expect_error(fetch_jwks("https://example.com/jwks"), "No keys found")
})

test_that("fetch_jwks skips keys without kid", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  jwk_with_kid <- jsondec(jose::write_jwk(pubkey))
  jwk_with_kid$kid <- "good-kid"
  jwk_no_kid <- jsondec(jose::write_jwk(pubkey))
  jwks_json <- jsonenc(list(keys = list(jwk_no_kid, jwk_with_kid)))

  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw(jwks_json),
      status = 200L,
      headers = list()
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- fetch_jwks("https://example.com/jwks")
  expect_equal(names(result$keys), "good-kid")
})

test_that("fetch_jwks skips unparseable keys", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  good_jwk <- jsondec(jose::write_jwk(pubkey))
  good_jwk$kid <- "good-kid"
  bad_jwk <- list(kid = "bad-kid", kty = "INVALID")
  jwks_json <- jsonenc(list(keys = list(bad_jwk, good_jwk)))

  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw(jwks_json),
      status = 200L,
      headers = list()
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- fetch_jwks("https://example.com/jwks")
  expect_equal(names(result$keys), "good-kid")
})

# ---- parse_query_params tests ----

test_that("parse_query_params extracts parameters", {
  result <- parse_query_params("/callback?code=abc&state=xyz")
  expect_equal(result$code, "abc")
  expect_equal(result$state, "xyz")
})

test_that("parse_query_params handles URL-encoded values", {
  result <- parse_query_params("/callback?msg=hello%20world&key=a%26b")
  expect_equal(result$msg, "hello world")
  expect_equal(result$key, "a&b")
})

test_that("parse_query_params returns empty list without query string", {
  expect_equal(parse_query_params("/callback"), list())
  expect_equal(parse_query_params("/"), list())
})

# ---- validate_token edge cases ----

test_that("validate_token returns error when get_signing_key throws", {
  key <- openssl::rsa_keygen(2048)
  claims <- valid_claims()
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) stop("network error")
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Unable to verify token signature")
})

test_that("validate_token rejects JWT header without kid", {
  # Create a JWT-like token with a valid base64url header that has no kid
  header_json <- '{"alg":"RS256","typ":"JWT"}'
  header_b64 <- jose::base64url_encode(charToRaw(header_json))
  token <- paste(header_b64, "payload", "signature", sep = ".")

  result <- validate_token(
    token = token,
    issuer = "https://accounts.google.com",
    client_id = "test-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Invalid JWT header")
})

# ---- validate_token missing exp ----

test_that("validate_token rejects JWT with missing exp claim", {
  key <- openssl::rsa_keygen(2048)
  pubkey <- as.list(key)$pubkey
  claims <- valid_claims()
  claims$exp <- NULL
  jwt <- create_test_jwt(claims, key)

  local_mocked_bindings(
    get_signing_key = function(issuer, kid) pubkey
  )

  result <- validate_token(
    token = jwt,
    issuer = "https://accounts.google.com",
    client_id = "test-client-id"
  )
  expect_false(result$valid)
  expect_equal(result$error, "Token expired")
})

# ---- amsync_token tests ----

# Helper: mock functions for the amsync_token OAuth flow.
# Returns nanonext mock functions that capture the callback and state, and
# a run_now mock that optionally simulates a callback request.
#
# Usage: call this to get mock fns, then call local_mocked_bindings() directly
# in the test (so mocks scope to the test frame).
make_token_mocks <- function(
  ncurl_token_resp = list(
    data = charToRaw(secretbase::jsonenc(list(id_token = "mock.jwt.token"))),
    status = 200L
  ),

  simulate_callback = TRUE,
  callback_params = NULL
) {
  env <- new.env(parent = emptyenv())

  oidc_config <- secretbase::jsonenc(list(
    authorization_endpoint = "https://auth.example.com/authorize",
    token_endpoint = "https://auth.example.com/token"
  ))

  ncurl_call <- 0L
  env$ncurl <- function(url, ...) {
    ncurl_call <<- ncurl_call + 1L
    if (ncurl_call == 1L) {
      list(data = charToRaw(oidc_config), status = 200L)
    } else {
      ncurl_token_resp
    }
  }

  env$handler <- function(path, callback) {
    env$captured_callback <- callback
    list()
  }

  env$browseURL <- function(url) {
    env$captured_state <- sub(".*state=([^&]+).*", "\\1", url)
  }

  run_count <- 0L
  env$run_now <- function(secs) {
    run_count <<- run_count + 1L
    if (run_count == 1L && simulate_callback &&
        !is.null(env$captured_callback) && !is.null(env$captured_state)) {
      params <- callback_params %||%
        paste0("/?code=test_auth_code&state=", env$captured_state)
      env$captured_callback(list(uri = params))
    }
  }

  env
}

# Helper: apply all token mocks in the calling test frame.
mock_token_flow <- function(
  ncurl_token_resp = list(
    data = charToRaw(secretbase::jsonenc(list(id_token = "mock.jwt.token"))),
    status = 200L
  ),
  simulate_callback = TRUE,
  callback_params = NULL
) {
  m <- make_token_mocks(ncurl_token_resp, simulate_callback, callback_params)
  list(
    nanonext = list(
      ncurl = m$ncurl,
      is_error_value = function(x) FALSE,
      parse_url = function(uri) {
        list(scheme = "http", hostname = "127.0.0.1", port = "0", path = "/")
      },
      handler = m$handler,
      http_server = function(url, handlers) {
        list(start = function() NULL, close = function() NULL)
      }
    ),
    autosync = list(
      is_interactive = function() TRUE,
      run_now = m$run_now
    ),
    utils = list(
      browseURL = m$browseURL
    )
  )
}

test_that("amsync_token errors in non-interactive session", {
  local_mocked_bindings(
    is_interactive = function() FALSE
  )
  expect_error(
    amsync_token(client_id = "test-id"),
    "requires an interactive session"
  )
})

test_that("amsync_token errors with invalid client_id", {
  local_mocked_bindings(
    is_interactive = function() TRUE
  )
  expect_error(amsync_token(client_id = ""), "'client_id' must be set")
  expect_error(amsync_token(client_id = 123), "'client_id' must be set")
})

test_that("amsync_token errors on failed OIDC discovery", {
  local_mocked_bindings(
    is_interactive = function() TRUE
  )
  local_mocked_bindings(
    ncurl = function(...) list(
      data = structure(5L, class = "errorValue"),
      status = 0L
    ),
    is_error_value = function(x) inherits(x, "errorValue"),
    .package = "nanonext"
  )

  expect_error(
    amsync_token(
      client_id = "test-id",
      issuer = "https://bad-issuer.com"
    ),
    "Failed to fetch OIDC configuration"
  )
})

test_that("amsync_token errors on missing OIDC endpoints", {
  local_mocked_bindings(
    is_interactive = function() TRUE
  )
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"issuer":"https://example.com"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  expect_error(
    amsync_token(client_id = "test-id", issuer = "https://example.com"),
    "missing authorization_endpoint or token_endpoint"
  )
})

test_that("amsync_token succeeds with valid OAuth flow", {
  m <- mock_token_flow()
  local_mocked_bindings(
    ncurl = m$nanonext$ncurl, is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  result <- amsync_token(
    client_id = "test-client",
    client_secret = "",
    issuer = "https://issuer.example.com",
    timeout = 2
  )

  expect_equal(result, "mock.jwt.token")
})

test_that("amsync_token errors on auth provider error", {
  m <- mock_token_flow(
    callback_params = "/?error=access_denied&error_description=User%20denied%20access"
  )
  local_mocked_bindings(
    ncurl = m$nanonext$ncurl, is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  expect_error(
    amsync_token(
      client_id = "test-client", client_secret = "",
      issuer = "https://issuer.example.com", timeout = 2
    ),
    "Authentication failed: User denied access"
  )
})

test_that("amsync_token errors on auth provider error without description", {
  m <- mock_token_flow(callback_params = "/?error=server_error")
  local_mocked_bindings(
    ncurl = m$nanonext$ncurl, is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  expect_error(
    amsync_token(
      client_id = "test-client", client_secret = "",
      issuer = "https://issuer.example.com", timeout = 2
    ),
    "Authentication failed: server_error"
  )
})

test_that("amsync_token errors on missing authorization code", {
  m <- mock_token_flow(callback_params = "/?other=value")
  local_mocked_bindings(
    ncurl = m$nanonext$ncurl, is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  expect_error(
    amsync_token(
      client_id = "test-client", client_secret = "",
      issuer = "https://issuer.example.com", timeout = 2
    ),
    "Authentication failed: No authorization code received"
  )
})

test_that("amsync_token errors on state mismatch", {
  m <- mock_token_flow(callback_params = "/?code=abc&state=wrong_state")
  local_mocked_bindings(
    ncurl = m$nanonext$ncurl, is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  expect_error(
    amsync_token(
      client_id = "test-client", client_secret = "",
      issuer = "https://issuer.example.com", timeout = 2
    ),
    "Authentication failed: State mismatch"
  )
})

test_that("amsync_token errors on timeout", {
  m <- mock_token_flow(simulate_callback = FALSE)
  local_mocked_bindings(
    ncurl = m$nanonext$ncurl, is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  expect_error(
    amsync_token(
      client_id = "test-client", client_secret = "",
      issuer = "https://issuer.example.com", timeout = 0
    ),
    "Authentication timed out"
  )
})

test_that("amsync_token errors on token exchange failure", {
  m <- mock_token_flow(ncurl_token_resp = list(
    data = charToRaw(secretbase::jsonenc(list(
      error = "invalid_grant", error_description = "Code expired"
    ))),
    status = 400L
  ))
  local_mocked_bindings(
    ncurl = m$nanonext$ncurl, is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  expect_error(
    amsync_token(
      client_id = "test-client", client_secret = "",
      issuer = "https://issuer.example.com", timeout = 2
    ),
    "Token exchange failed: Code expired"
  )
})

test_that("amsync_token errors on token exchange with non-JSON response", {
  m <- mock_token_flow(ncurl_token_resp = list(
    data = charToRaw("Internal Server Error"), status = 500L
  ))
  local_mocked_bindings(
    ncurl = m$nanonext$ncurl, is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  expect_error(
    amsync_token(
      client_id = "test-client", client_secret = "",
      issuer = "https://issuer.example.com", timeout = 2
    ),
    "Token exchange failed"
  )
})

test_that("amsync_token errors when response has no id_token", {
  m <- mock_token_flow(ncurl_token_resp = list(
    data = charToRaw(secretbase::jsonenc(list(access_token = "at"))),
    status = 200L
  ))
  local_mocked_bindings(
    ncurl = m$nanonext$ncurl, is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  expect_error(
    amsync_token(
      client_id = "test-client", client_secret = "",
      issuer = "https://issuer.example.com", timeout = 2
    ),
    "No ID token in response"
  )
})

test_that("amsync_token includes client_secret when provided", {
  captured_token_data <- NULL
  m <- mock_token_flow()

  # Override ncurl to capture token exchange data
  original_ncurl <- m$nanonext$ncurl
  ncurl_call <- 0L
  oidc_config <- secretbase::jsonenc(list(
    authorization_endpoint = "https://auth.example.com/authorize",
    token_endpoint = "https://auth.example.com/token"
  ))
  local_mocked_bindings(
    ncurl = function(url, ...) {
      ncurl_call <<- ncurl_call + 1L
      if (ncurl_call == 1L) {
        list(data = charToRaw(oidc_config), status = 200L)
      } else {
        args <- list(...)
        captured_token_data <<- args$data
        list(
          data = charToRaw(secretbase::jsonenc(list(id_token = "jwt"))),
          status = 200L
        )
      }
    },
    is_error_value = m$nanonext$is_error_value,
    parse_url = m$nanonext$parse_url, handler = m$nanonext$handler,
    http_server = m$nanonext$http_server, .package = "nanonext"
  )
  local_mocked_bindings(
    is_interactive = m$autosync$is_interactive, run_now = m$autosync$run_now
  )
  local_mocked_bindings(browseURL = m$utils$browseURL, .package = "utils")

  amsync_token(
    client_id = "test-client",
    client_secret = "my-secret",
    issuer = "https://issuer.example.com",
    timeout = 2
  )

  expect_true(grepl("client_secret=my-secret", captured_token_data))
})

# ---- server integration tests ----

test_that("server requires TLS when auth is enabled", {
  expect_snapshot(
    amsync_server(
      auth = auth_config(
        issuer = "https://accounts.google.com",
        client_id = "test-id"
      )
    ),
    error = TRUE
  )
})

test_that("server allows auth with TLS configured", {
  skip_on_cran()

  cert <- nanonext::write_cert()
  tls <- nanonext::tls_config(server = cert$server)

  server <- amsync_server(
    tls = tls,
    auth = auth_config(
      issuer = "https://accounts.google.com",
      client_id = "test-id",
      allowed_emails = "test@example.com"
    )
  )
  on.exit(server$close())

  expect_s3_class(server, "amsync_server")
  state <- attr(server, "sync")
  expect_s3_class(state$auth, "amsync_auth_config")
})

test_that("server rejects unauthenticated client when auth enabled", {
  skip_on_cran()

  cert <- nanonext::write_cert()
  tls <- nanonext::tls_config(server = cert$server)
  client_tls <- nanonext::tls_config(client = cert$client)

  server <- amsync_server(
    tls = tls,
    auth = auth_config(
      issuer = "https://accounts.google.com",
      client_id = "test-id",
      allowed_emails = "allowed@test.com"
    )
  )
  on.exit(server$close())
  server$start()

  # Client without token should be rejected
  expect_snapshot(
    amsync_fetch(
      url = server$url,
      doc_id = generate_document_id(),
      tls = client_tls
    ),
    error = TRUE
  )
})
