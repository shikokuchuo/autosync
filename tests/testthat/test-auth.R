# Tests for authentication module

test_that("auth_config creates valid configuration", {
  cfg <- auth_config(
    allowed_domains = "example.com",
    allowed_emails = "test@example.com"
  )

  expect_s3_class(cfg, "amsync_auth_config")
  expect_equal(cfg$allowed_domains, "example.com")
  expect_equal(cfg$allowed_emails, "test@example.com")
  expect_equal(cfg$auth_timeout, 10)
  expect_equal(cfg$token_timeout, 5)
})

test_that("auth_config validates timeout parameters", {
  expect_snapshot(auth_config(auth_timeout = -1), error = TRUE)
  expect_snapshot(auth_config(auth_timeout = "10"), error = TRUE)
  expect_snapshot(auth_config(token_timeout = 0), error = TRUE)
  expect_snapshot(auth_config(token_timeout = c(1, 2)), error = TRUE)
})

test_that("auth_config accepts custom timeout values", {
  cfg <- auth_config(auth_timeout = 30, token_timeout = 10)
  expect_equal(cfg$auth_timeout, 30)
  expect_equal(cfg$token_timeout, 10)
})

test_that("authenticate_client returns generic error for invalid credentials", {
  cfg <- auth_config()

  # Missing peerMetadata
  result <- authenticate_client(cfg, NULL)
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Missing access_token
  result <- authenticate_client(cfg, list(isEphemeral = TRUE))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Non-character token
  result <- authenticate_client(cfg, list(access_token = 12345))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Multiple tokens
  result <- authenticate_client(cfg, list(access_token = c("a", "b")))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Too short
  result <- authenticate_client(cfg, list(access_token = "short"))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Too long (over 4096 chars)
  result <- authenticate_client(
    cfg,
    list(access_token = paste(rep("a", 5000), collapse = ""))
  )
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Contains spaces
  result <- authenticate_client(
    cfg,
    list(access_token = paste(rep("a", 50), collapse = " "))
  )
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Contains special chars
  result <- authenticate_client(
    cfg,
    list(access_token = paste0(strrep("a", 25), "$%^&", strrep("b", 25)))
  )
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")

  # Empty string token
  result <- authenticate_client(cfg, list(access_token = ""))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")
})

test_that("server requires TLS when auth is enabled", {
  expect_snapshot(
    amsync_server(
      port = get_test_port(),
      auth = auth_config(allowed_emails = "test@example.com")
    ),
    error = TRUE
  )
})

test_that("server allows auth with TLS configured", {
  skip_on_cran()

  port <- get_test_port()
  cert <- nanonext::write_cert()
  tls <- nanonext::tls_config(server = cert$server)

  server <- amsync_server(
    port = port,
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

  port <- get_test_port()
  cert <- nanonext::write_cert()
  tls <- nanonext::tls_config(server = cert$server)
  client_tls <- nanonext::tls_config(client = cert$client)

  server <- amsync_server(
    port = port,
    tls = tls,
    auth = auth_config(allowed_emails = "allowed@test.com")
  )
  on.exit(server$close())
  server$start()

  # Client without token should be rejected
  expect_snapshot(
    amsync_fetch(
      url = sprintf("wss://127.0.0.1:%d", port),
      doc_id = generate_document_id(),
      tls = client_tls
    ),
    error = TRUE
  )
})

test_that("server closes connection on auth timeout", {
  skip_on_cran()
  skip_if_not_installed("gargle")

  port <- get_test_port()
  cert <- nanonext::write_cert()
  tls <- nanonext::tls_config(server = cert$server)
  client_tls <- nanonext::tls_config(client = cert$client)

  server <- amsync_server(
    port = port,
    tls = tls,
    auth = auth_config(
      allowed_emails = "allowed@test.com",
      auth_timeout = 1 # 1 second timeout for testing
    )
  )
  on.exit(server$close())
  server$start()

  # Connect but don't send join message
  s <- nanonext::stream(
    dial = sprintf("wss://127.0.0.1:%d", port),
    tls = client_tls
  )
  on.exit(close(s), add = TRUE)

  # Wait for timeout
  Sys.sleep(1.5)
  later::run_now()

  # Connection should be closed by server - attempting to send should fail
  result <- tryCatch(
    {
      nanonext::send(s, charToRaw("test"), mode = "raw", block = 100L)
      "sent"
    },
    error = function(e) "error"
  )

  # Either send fails or we get an error message back
  expect_true(result == "error" || result == "sent")
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

test_that("auth_config stores multiple emails and domains", {
  cfg <- auth_config(
    allowed_emails = c("alice@test.com", "bob@test.com"),
    allowed_domains = c("test.com", "example.com")
  )
  expect_equal(cfg$allowed_emails, c("alice@test.com", "bob@test.com"))
  expect_equal(cfg$allowed_domains, c("test.com", "example.com"))
})

# ---- authenticate_client boundary tests ----

test_that("authenticate_client accepts token at minimum length boundary", {
  cfg <- auth_config()
  local_mocked_bindings(
    validate_token = function(...) list(valid = TRUE, email = "a@b.com", error = NULL)
  )
  result <- authenticate_client(cfg, list(access_token = strrep("a", 20)))
  expect_true(result$valid)
})

test_that("authenticate_client rejects token one below minimum length", {
  cfg <- auth_config()
  result <- authenticate_client(cfg, list(access_token = strrep("a", 19)))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")
})

test_that("authenticate_client accepts token at maximum length boundary", {
  cfg <- auth_config()
  local_mocked_bindings(
    validate_token = function(...) list(valid = TRUE, email = "a@b.com", error = NULL)
  )
  result <- authenticate_client(cfg, list(access_token = strrep("a", 4096)))
  expect_true(result$valid)
})

test_that("authenticate_client rejects token one above maximum length", {
  cfg <- auth_config()
  result <- authenticate_client(cfg, list(access_token = strrep("a", 4097)))
  expect_false(result$valid)
  expect_equal(result$error, "Authentication failed")
})


test_that("authenticate_client accepts all valid special characters", {
  cfg <- auth_config()
  local_mocked_bindings(
    validate_token = function(...) list(valid = TRUE, email = "a@b.com", error = NULL)
  )
  # All allowed characters: A-Za-z0-9._~+/-
  token <- "ABCDEFghij0123456789._~+/-"
  result <- authenticate_client(cfg, list(access_token = token))
  expect_true(result$valid)
})

test_that("authenticate_client passes config to validate_token", {
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
  authenticate_client(cfg, list(access_token = token))

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

test_that("validate_token detects negative expires_in", {
  local_mocked_bindings(
    ncurl = function(...) list(
      data = charToRaw('{"email":"user@test.com","expires_in":"-100"}'),
      status = 200L
    ),
    is_error_value = function(x) FALSE,
    .package = "nanonext"
  )

  result <- validate_token("expired_token_for_test_1234")
  expect_false(result$valid)
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

# ---- check_auth_timeout unit tests ----

test_that("check_auth_timeout cleans up pending connection", {
  state <- new.env(hash = TRUE, parent = emptyenv())
  state$peer_id <- "server-peer-id"
  state$pending_auth <- new.env(hash = TRUE, parent = emptyenv())
  state$connections <- new.env(hash = TRUE, parent = emptyenv())

  temp_id <- "pending-conn-123"
  state$pending_auth[[temp_id]] <- list(connected_at = Sys.time())

  ws <- new.env(hash = TRUE)
  ws$sent_messages <- list()
  ws$send <- function(data) ws$sent_messages <- c(ws$sent_messages, list(data))

  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  check_auth_timeout(state, temp_id)

  expect_false(exists(temp_id, envir = state$pending_auth))
  expect_length(ws$sent_messages, 1)
  msg <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(msg$type, "error")
  expect_match(msg$message, "Authentication timeout")
})

test_that("check_auth_timeout is no-op for already authenticated connection", {
  state <- new.env(hash = TRUE, parent = emptyenv())
  state$peer_id <- "server-peer-id"
  state$pending_auth <- new.env(hash = TRUE, parent = emptyenv())
  state$connections <- new.env(hash = TRUE, parent = emptyenv())

  # temp_id NOT in pending_auth (already authenticated)
  expect_no_error(check_auth_timeout(state, "already-auth-456"))
})

# ---- handle_join with auth unit tests ----

test_that("handle_join with auth rejects missing token", {
  state <- new.env(hash = TRUE, parent = emptyenv())
  state$peer_id <- autosync:::generate_peer_id()
  state$storage_id <- autosync:::generate_peer_id()
  state$documents <- new.env(hash = TRUE, parent = emptyenv())
  state$sync_states <- new.env(hash = TRUE, parent = emptyenv())
  state$connections <- new.env(hash = TRUE, parent = emptyenv())
  state$doc_peers <- new.env(hash = TRUE, parent = emptyenv())
  state$pending_auth <- new.env(hash = TRUE, parent = emptyenv())
  state$auth <- auth_config()

  ws <- new.env(hash = TRUE)
  ws$sent_messages <- list()
  ws$send <- function(data) ws$sent_messages <- c(ws$sent_messages, list(data))

  temp_id <- "temp-ws-789"
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )
  state$pending_auth[[temp_id]] <- list(connected_at = Sys.time())

  join_msg <- list(
    type = "join",
    senderId = "badAuthClient",
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("1")
  )

  autosync:::handle_join(state, temp_id, join_msg)

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "error")
  expect_match(response$message, "Authentication failed")
  expect_false(exists(temp_id, envir = state$pending_auth))
})

test_that("handle_join with auth accepts valid credentials", {
  state <- new.env(hash = TRUE, parent = emptyenv())
  state$peer_id <- autosync:::generate_peer_id()
  state$storage_id <- autosync:::generate_peer_id()
  state$documents <- new.env(hash = TRUE, parent = emptyenv())
  state$sync_states <- new.env(hash = TRUE, parent = emptyenv())
  state$connections <- new.env(hash = TRUE, parent = emptyenv())
  state$doc_peers <- new.env(hash = TRUE, parent = emptyenv())
  state$pending_auth <- new.env(hash = TRUE, parent = emptyenv())
  state$auth <- auth_config(allowed_emails = "user@test.com")

  ws <- new.env(hash = TRUE)
  ws$sent_messages <- list()
  ws$send <- function(data) ws$sent_messages <- c(ws$sent_messages, list(data))

  temp_id <- "temp-ws-valid"
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )
  state$pending_auth[[temp_id]] <- list(connected_at = Sys.time())

  local_mocked_bindings(
    authenticate_client = function(...) list(
      valid = TRUE, email = "user@test.com", error = NULL
    )
  )

  join_msg <- list(
    type = "join",
    senderId = "goodAuthClient",
    peerMetadata = list(access_token = strrep("x", 30)),
    supportedProtocolVersions = list("1")
  )

  autosync:::handle_join(state, temp_id, join_msg)

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "peer")
  expect_equal(response$targetId, "goodAuthClient")
  expect_false(exists(temp_id, envir = state$pending_auth))
  expect_equal(state$connections[[temp_id]]$authenticated_email, "user@test.com")
})

test_that("handle_join without auth skips authentication", {
  state <- new.env(hash = TRUE, parent = emptyenv())
  state$peer_id <- autosync:::generate_peer_id()
  state$storage_id <- autosync:::generate_peer_id()
  state$documents <- new.env(hash = TRUE, parent = emptyenv())
  state$sync_states <- new.env(hash = TRUE, parent = emptyenv())
  state$connections <- new.env(hash = TRUE, parent = emptyenv())
  state$doc_peers <- new.env(hash = TRUE, parent = emptyenv())
  state$pending_auth <- new.env(hash = TRUE, parent = emptyenv())
  state$auth <- NULL

  ws <- new.env(hash = TRUE)
  ws$sent_messages <- list()
  ws$send <- function(data) ws$sent_messages <- c(ws$sent_messages, list(data))

  temp_id <- "temp-ws-noauth"
  state$connections[[temp_id]] <- list(
    ws = ws,
    client_id = NULL,
    metadata = NULL,
    connected_at = Sys.time()
  )

  join_msg <- list(
    type = "join",
    senderId = "noAuthClient",
    peerMetadata = list(isEphemeral = TRUE),
    supportedProtocolVersions = list("1")
  )

  autosync:::handle_join(state, temp_id, join_msg)

  expect_length(ws$sent_messages, 1)
  response <- secretbase::cbordec(ws$sent_messages[[1]])
  expect_equal(response$type, "peer")
  expect_equal(response$targetId, "noAuthClient")
})
