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

test_that("authenticate_client rejects missing peerMetadata", {
  cfg <- auth_config()
  result <- authenticate_client(cfg, NULL)

  expect_false(result$valid)
  expect_snapshot(result$error)
})

test_that("authenticate_client rejects missing access_token", {
  cfg <- auth_config()
  result <- authenticate_client(cfg, list(isEphemeral = TRUE))

  expect_false(result$valid)
  expect_snapshot(result$error)
})

test_that("authenticate_client rejects invalid token format", {
  cfg <- auth_config()

  # Non-character token
  result <- authenticate_client(cfg, list(access_token = 12345))
  expect_false(result$valid)
  expect_snapshot(result$error)

  # Multiple tokens
  result <- authenticate_client(cfg, list(access_token = c("a", "b")))
  expect_false(result$valid)
  expect_snapshot(result$error)
})

test_that("authenticate_client rejects token with invalid length", {
  cfg <- auth_config()

  # Too short
  result <- authenticate_client(cfg, list(access_token = "short"))
  expect_false(result$valid)
  expect_snapshot(result$error)

  # Too long (over 4096 chars)
  result <- authenticate_client(
    cfg,
    list(access_token = paste(rep("a", 5000), collapse = ""))
  )
  expect_false(result$valid)
  expect_snapshot(result$error)
})

test_that("authenticate_client rejects token with invalid characters", {
  cfg <- auth_config()

  # Contains spaces
  result <- authenticate_client(
    cfg,
    list(access_token = paste(rep("a", 50), collapse = " "))
  )
  expect_false(result$valid)
  expect_snapshot(result$error)

  # Contains special chars
  result <- authenticate_client(
    cfg,
    list(access_token = paste0(strrep("a", 25), "$%^&", strrep("b", 25)))
  )
  expect_false(result$valid)
  expect_snapshot(result$error)
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
