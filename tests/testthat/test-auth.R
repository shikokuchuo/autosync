# Tests for authentication module

test_that("auth_config creates valid configuration", {
  cfg <- auth_config(
    allowed_domains = "example.com",
    allowed_emails = "test@example.com"
  )

  expect_s3_class(cfg, "amsync_auth_config")
  expect_equal(cfg$allowed_domains, "example.com")
  expect_equal(cfg$allowed_emails, "test@example.com")
  expect_equal(cfg$token_timeout, 5)
})

test_that("auth_config validates timeout parameters", {
  expect_snapshot(auth_config(token_timeout = 0), error = TRUE)
  expect_snapshot(auth_config(token_timeout = c(1, 2)), error = TRUE)
})

test_that("auth_config accepts custom timeout values", {
  cfg <- auth_config(token_timeout = 10)
  expect_equal(cfg$token_timeout, 10)
})

test_that("authenticate_header returns generic error for invalid credentials", {
  cfg <- auth_config()

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

  # Too long (over 4096 chars)
  result <- authenticate_header(
    cfg,
    c(Authorization = paste("Bearer", strrep("a", 5000)))
  )
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Contains special chars
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

test_that("server requires TLS when auth is enabled", {
  expect_snapshot(
    amsync_server(
      auth = auth_config(allowed_emails = "test@example.com")
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
    auth = auth_config(allowed_emails = "test@example.com")
  )
  on.exit(server$close())

  expect_s3_class(server, "amsync_server")
  state <- attr(server, "sync")
  expect_s3_class(state$auth, "amsync_auth_config")
})

test_that("server rejects unauthenticated client when auth enabled", {
  skip_on_cran()
  skip_if_not_installed("gargle")

  cert <- nanonext::write_cert()
  tls <- nanonext::tls_config(server = cert$server)
  client_tls <- nanonext::tls_config(client = cert$client)

  server <- amsync_server(
    tls = tls,
    auth = auth_config(allowed_emails = "allowed@test.com")
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


test_that("validate_token handles invalid token gracefully", {
  # Test with a clearly invalid token - should get an error response from Google
  result <- validate_token("invalid_test_token_12345")
  expect_false(result$valid)
  expect_null(result$email)
  expect_type(result$error, "character")
})

# ---- auth_config additional tests ----

test_that("auth_config has NULL defaults for optional parameters", {
  cfg <- auth_config()
  expect_null(cfg$allowed_emails)
  expect_null(cfg$allowed_domains)
  expect_null(cfg$custom_validator)
})

test_that("auth_config stores custom_validator function", {
  validator <- function(token_info) token_info$audience == "my-app"
  cfg <- auth_config(custom_validator = validator)
  expect_identical(cfg$custom_validator, validator)
})

# ---- authenticate_header boundary tests ----

test_that("authenticate_header accepts token at minimum length boundary", {
  cfg <- auth_config()
  local_mocked_bindings(
    validate_token = function(...) list(valid = TRUE, email = "a@b.com", error = NULL)
  )
  result <- authenticate_header(cfg, c(Authorization = paste("Bearer", strrep("a", 20))))
  expect_true(result$valid)
})

test_that("authenticate_header accepts token at maximum length boundary", {
  cfg <- auth_config()
  local_mocked_bindings(
    validate_token = function(...) list(valid = TRUE, email = "a@b.com", error = NULL)
  )
  result <- authenticate_header(cfg, c(Authorization = paste("Bearer", strrep("a", 4096))))
  expect_true(result$valid)
})

test_that("authenticate_header accepts all valid special characters", {
  cfg <- auth_config()
  local_mocked_bindings(
    validate_token = function(...) list(valid = TRUE, email = "a@b.com", error = NULL)
  )
  # All allowed characters: A-Za-z0-9._~+/-
  token <- "ABCDEFghij0123456789._~+/-"
  result <- authenticate_header(cfg, c(Authorization = paste("Bearer", token)))
  expect_true(result$valid)
})

test_that("authenticate_header passes config to validate_token", {
  cfg <- auth_config(
    allowed_emails = c("user@test.com"),
    allowed_domains = c("test.com"),
    custom_validator = function(x) TRUE,
    token_timeout = 7
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

  expect_equal(captured_args$access_token, token)
  expect_equal(captured_args$allowed_emails, c("user@test.com"))
  expect_equal(captured_args$allowed_domains, c("test.com"))
  expect_true(is.function(captured_args$custom_validator))
  expect_equal(captured_args$token_timeout, 7)
})

# ---- validate_token mocked tests ----

test_that("validate_token handles network error from ncurl", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = structure(5L, class = "errorValue"),
      status = 0L
    ),
    is_error_value = function(x) inherits(x, "errorValue"),
    .package = "nanonext"
  )

  result <- validate_token("valid_looking_token_123")
  expect_false(result$valid)
  expect_null(result$email)
  expect_match(result$error, "Token validation failed:")
})

test_that("validate_token handles HTTP error with error_description", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"error_description":"Invalid Value","error":"invalid_token"}'),
      status = 400L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token("bad_token_for_testing_1234")
  expect_false(result$valid)
  expect_null(result$email)
  expect_equal(result$error, "Invalid Value")
})

test_that("validate_token handles HTTP error with error field only", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"error":"invalid_token"}'),
      status = 401L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token("bad_token_for_testing_1234")
  expect_false(result$valid)
  expect_equal(result$error, "invalid_token")
})

test_that("validate_token handles HTTP error with no error details", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{}'),
      status = 500L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token("bad_token_for_testing_1234")
  expect_false(result$valid)
  expect_equal(result$error, "Token validation failed (HTTP 500)")
})

test_that("validate_token detects expired token", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"user@test.com","expires_in":"0"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token("expired_token_for_test_1234")
  expect_false(result$valid)
  expect_null(result$email)
  expect_equal(result$error, "Token expired")
})

test_that("validate_token allows token without expires_in field", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"user@test.com"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token("valid_token_for_test_12345")
  expect_true(result$valid)
  expect_equal(result$email, "user@test.com")
  expect_null(result$error)
})

test_that("validate_token rejects email not in allowlist", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"stranger@other.com","expires_in":"3600"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token(
    "valid_token_for_test_12345",
    allowed_emails = c("alice@test.com", "bob@test.com")
  )
  expect_false(result$valid)
  expect_equal(result$email, "stranger@other.com")
  expect_equal(result$error, "Email not in allowlist")
})

test_that("validate_token accepts email in allowlist", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"alice@test.com","expires_in":"3600"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token(
    "valid_token_for_test_12345",
    allowed_emails = c("alice@test.com", "bob@test.com")
  )
  expect_true(result$valid)
  expect_equal(result$email, "alice@test.com")
})

test_that("validate_token rejects disallowed domain", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"user@evil.com","expires_in":"3600"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token(
    "valid_token_for_test_12345",
    allowed_domains = c("trusted.com", "partner.org")
  )
  expect_false(result$valid)
  expect_equal(result$email, "user@evil.com")
  expect_equal(result$error, "Domain not allowed")
})

test_that("validate_token accepts allowed domain", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"user@trusted.com","expires_in":"3600"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token(
    "valid_token_for_test_12345",
    allowed_domains = c("trusted.com", "partner.org")
  )
  expect_true(result$valid)
  expect_equal(result$email, "user@trusted.com")
})

test_that("validate_token rejects when custom_validator returns FALSE", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"user@test.com","expires_in":"3600","audience":"wrong-app"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token(
    "valid_token_for_test_12345",
    custom_validator = function(info) info$audience == "my-app"
  )
  expect_false(result$valid)
  expect_equal(result$email, "user@test.com")
  expect_equal(result$error, "Custom validation failed")
})

test_that("validate_token accepts when custom_validator returns TRUE", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"user@test.com","expires_in":"3600","audience":"my-app"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token(
    "valid_token_for_test_12345",
    custom_validator = function(info) info$audience == "my-app"
  )
  expect_true(result$valid)
  expect_equal(result$email, "user@test.com")
})

test_that("validate_token applies email, domain, and custom checks in order", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"user@trusted.com","expires_in":"3600"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token(
    "valid_token_for_test_12345",
    allowed_emails = c("user@trusted.com"),
    allowed_domains = c("trusted.com"),
    custom_validator = function(info) TRUE
  )
  expect_true(result$valid)
  expect_equal(result$email, "user@trusted.com")
})

# ---- amsync_auth tests ----

test_that("amsync_auth errors when gargle is not available", {
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) FALSE,
    .package = "base"
  )

  expect_error(amsync_auth(), "Package 'gargle' is required")
})

test_that("amsync_auth errors when token_fetch returns NULL", {
  skip_if_not_installed("gargle")

  local_mocked_bindings(
    token_fetch = function(...) NULL,
    .package = "gargle"
  )

  expect_error(amsync_auth(email = "test@example.com"), "Failed to obtain OAuth2 token")
})

test_that("amsync_auth returns access_token and passes args to token_fetch", {
  skip_if_not_installed("gargle")

  captured_args <- NULL
  local_mocked_bindings(
    token_fetch = function(...) {
      captured_args <<- list(...)
      list(credentials = list(access_token = "mock_access_token_12345"))
    },
    .package = "gargle"
  )

  result <- amsync_auth(
    email = "user@test.com",
    scopes = "https://www.googleapis.com/auth/userinfo.email"
  )

  expect_equal(result, "mock_access_token_12345")
  expect_equal(captured_args$email, "user@test.com")
  expect_equal(captured_args$scopes, "https://www.googleapis.com/auth/userinfo.email")
})

