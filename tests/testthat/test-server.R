test_that("amsync_server creates valid server object", {
  server <- amsync_server(port = get_test_port(), data_dir = tempdir())
  on.exit(server$stop())

  state <- attr(server, "sync")
  expect_s3_class(server, "amsync_server")
  expect_type(state$peer_id, "character")
  expect_type(state$storage_id, "character")
})

test_that("amsync_server with ephemeral storage_id", {
  server <- amsync_server(port = get_test_port(), storage_id = NA, data_dir = tempdir())
  on.exit(server$stop())

  state <- attr(server, "sync")
  expect_null(state$storage_id)
})

test_that("create_document generates valid ID", {
  server <- amsync_server(port = get_test_port(), data_dir = tempdir())
  on.exit({
    server$stop()
    unlink(file.path(tempdir(), "*.automerge"))
  })

  doc_id <- create_document(server)
  expect_type(doc_id, "character")
  expect_true(nchar(doc_id) > 20)  # Base58Check encoded 16 bytes + version + checksum
  expect_true(doc_id %in% list_documents(server))
})

test_that("get_document retrieves created document", {
  server <- amsync_server(port = get_test_port(), data_dir = tempdir())
  on.exit({
    server$stop()
    unlink(file.path(tempdir(), "*.automerge"))
  })

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  expect_true(inherits(doc, "am_doc"))
})

test_that("list_documents returns all documents", {
  server <- amsync_server(port = get_test_port(), data_dir = tempdir())
  on.exit({
    server$stop()
    unlink(file.path(tempdir(), "*.automerge"))
  })

  expect_length(list_documents(server), 0)

  id1 <- create_document(server)
  id2 <- create_document(server)

  docs <- list_documents(server)
  expect_length(docs, 2)
  expect_true(id1 %in% docs)
  expect_true(id2 %in% docs)
})

test_that("print.amsync_server works", {
  port <- get_test_port()
  server <- amsync_server(port = port, data_dir = tempdir())
  on.exit(server$stop())

  output <- capture.output(print(server))
  expect_true(any(grepl("Automerge Sync Server", output)))
  expect_true(any(grepl(paste0("Port: ", port), output)))
})

test_that("generate_document_id creates valid IDs", {
  id <- generate_document_id()
  expect_type(id, "character")
  expect_true(nchar(id) %in% 27:28)
  # Should be decodable to 16 bytes
  bytes <- secretbase::base58dec(id, convert = FALSE)
  expect_length(bytes, 16L)
})

test_that("amsync_server with TLS creates wss URL", {
  server <- amsync_server(
    port = get_test_port(),
    tls = nanonext::tls_config(server = nanonext::write_cert()$server),
    data_dir = tempdir()
  )
  on.exit(server$stop())

  expect_true(grepl("^wss://", server$url))
})

test_that("amsync_server without TLS creates ws URL", {
  server <- amsync_server(port = get_test_port(), data_dir = tempdir())
  on.exit(server$stop())

  expect_true(grepl("^ws://", server$url))
})

test_that("amsync_server with custom storage_id", {
  server <- amsync_server(
    port = get_test_port(),
    storage_id = "customStorageId",
    data_dir = tempdir()
  )
  on.exit(server$stop())

  state <- attr(server, "sync")
  expect_equal(state$storage_id, "customStorageId")
})
