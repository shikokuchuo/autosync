# Integration tests that don't require async callbacks

test_that("document persistence survives server restart", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  # Create server, add document
  server1 <- amsync_server(data_dir = data_dir)

  doc_id <- create_document(server1)
  doc <- get_document(server1, doc_id)
  automerge::am_put(doc, automerge::AM_ROOT, "persistent", "data")
  autosync:::save_document(attr(server1, "sync"), doc_id, doc)

  server1$close()

  # Create new server with same data_dir
  server2 <- amsync_server(data_dir = data_dir)
  on.exit(server2$close(), add = TRUE)

  # Document should be loaded
  expect_true(doc_id %in% list_documents(server2))

  doc2 <- get_document(server2, doc_id)
  expect_equal(automerge::am_get(doc2, automerge::AM_ROOT, "persistent"), "data")
})

test_that("server creates data directory if it doesn't exist", {
  data_dir <- file.path(tempfile(), "nested", "path")
  on.exit(unlink(dirname(dirname(data_dir)), recursive = TRUE))

  expect_false(dir.exists(data_dir))

  server <- amsync_server(data_dir = data_dir)
  on.exit(server$close(), add = TRUE)

  expect_true(dir.exists(data_dir))
})

test_that("multiple documents can be managed simultaneously", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  on.exit(server$close(), add = TRUE)

  # Create multiple documents
  doc_ids <- sapply(1:5, function(i) {
    doc_id <- create_document(server)
    doc <- get_document(server, doc_id)
    automerge::am_put(doc, automerge::AM_ROOT, "index", i)
    doc_id
  })

  # Verify each document
  for (i in 1:5) {
    doc <- get_document(server, doc_ids[i])
    expect_equal(automerge::am_get(doc, automerge::AM_ROOT, "index"), i)
  }
})

test_that("get_document returns NULL for non-existent document", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  on.exit(server$close())

  result <- get_document(server, "nonexistent123")
  expect_null(result)
})

test_that("create_document with explicit ID", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  on.exit(server$close())

  explicit_id <- generate_document_id()
  returned_id <- create_document(server, doc_id = explicit_id)

  expect_equal(returned_id, explicit_id)
  expect_true(explicit_id %in% list_documents(server))
})

test_that("server URL format is correct", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(host = "127.0.0.1", data_dir = data_dir)
  on.exit(server$close())

  expect_true(grepl("^ws://127\\.0\\.0\\.1:\\d+$", server$url))
})

test_that("server state is accessible via attribute", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  on.exit(server$close())

  state <- attr(server, "sync")

  expect_type(state$peer_id, "character")
  expect_type(state$storage_id, "character")
  expect_true(inherits(state$documents, "environment"))
  expect_true(inherits(state$sync_states, "environment"))
  expect_true(inherits(state$connections, "environment"))
})
