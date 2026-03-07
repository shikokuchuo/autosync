test_that("generate_peer_id creates valid Base64 encoded IDs", {
  id <- autosync:::generate_peer_id()
  expect_type(id, "character")
  expect_true(nchar(id) > 0)
  # Should be decodable as Base64 to 16 bytes
  bytes <- secretbase::base64dec(id, convert = FALSE)
  expect_length(bytes, 16L)
})

test_that("generate_peer_id creates unique IDs", {
  ids <- replicate(100, autosync:::generate_peer_id())
  expect_equal(length(unique(ids)), 100)
})

test_that("%||% returns x when not NULL", {
  `%||%` <- autosync:::`%||%`
  expect_equal(5 %||% 10, 5)
  expect_equal("a" %||% "b", "a")
  expect_equal(list(x = 1) %||% list(y = 2), list(x = 1))
  expect_equal(FALSE %||% TRUE, FALSE)
  expect_equal(0 %||% 1, 0)
})

test_that("%||% returns y when x is NULL", {
  `%||%` <- autosync:::`%||%`
  expect_equal(NULL %||% 10, 10)
  expect_equal(NULL %||% "default", "default")
  expect_equal(NULL %||% list(a = 1), list(a = 1))
})

test_that("generate_document_id creates unique IDs", {
  ids <- replicate(100, generate_document_id())
  expect_equal(length(unique(ids)), 100)
})

# ---- close_connection tests ----

test_that("close_connection closes ws for existing connection", {
  server <- new.env(hash = TRUE, parent = emptyenv())
  server$connections <- new.env(hash = TRUE, parent = emptyenv())

  ws <- new.env(hash = TRUE)
  ws$closed <- FALSE
  ws$close <- function() ws$closed <- TRUE

  server$connections[["conn1"]] <- list(ws = ws, client_id = "conn1")

  autosync:::close_connection(server, "conn1")
  expect_true(ws$closed)
})

test_that("close_connection handles missing connection gracefully", {
  server <- new.env(hash = TRUE, parent = emptyenv())
  server$connections <- new.env(hash = TRUE, parent = emptyenv())

  expect_no_error(autosync:::close_connection(server, "nonexistent"))
})

test_that("close_connection handles connection with NULL ws", {
  server <- new.env(hash = TRUE, parent = emptyenv())
  server$connections <- new.env(hash = TRUE, parent = emptyenv())

  server$connections[["conn2"]] <- list(ws = NULL, client_id = "conn2")

  expect_no_error(autosync:::close_connection(server, "conn2"))
})
