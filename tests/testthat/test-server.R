test_that("amsync_server creates valid server object", {
  server <- amsync_server(port = 0, data_dir = tempdir())
  on.exit(stop_server(server))

  expect_s3_class(server, "amsync_server")
  expect_false(server$running)
  expect_type(server$peer_id, "character")
  expect_type(server$storage_id, "character")
})

test_that("amsync_server with ephemeral storage_id", {
  server <- amsync_server(port = 0, storage_id = NA, data_dir = tempdir())
  on.exit(stop_server(server))

  expect_null(server$storage_id)
})

test_that("create_document generates valid ID", {
  server <- amsync_server(port = 0, data_dir = tempdir())
  on.exit({
    stop_server(server)
    unlink(file.path(tempdir(), "*.automerge"))
  })

  doc_id <- create_document(server)
  expect_type(doc_id, "character")
  expect_true(nchar(doc_id) > 20)  # Base58Check encoded 16 bytes + version + checksum
  expect_true(doc_id %in% list_documents(server))
})

test_that("get_document retrieves created document", {
  server <- amsync_server(port = 0, data_dir = tempdir())
  on.exit({
    stop_server(server)
    unlink(file.path(tempdir(), "*.automerge"))
  })

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  expect_true(inherits(doc, "am_doc"))
})

test_that("list_documents returns all documents", {
  server <- amsync_server(port = 0, data_dir = tempdir())
  on.exit({
    stop_server(server)
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
  server <- amsync_server(port = 3030, data_dir = tempdir())
  on.exit(stop_server(server))

  output <- capture.output(print(server))
  expect_true(any(grepl("Automerge Sync Server", output)))
  expect_true(any(grepl("Port: 3030", output)))
})

test_that("generate_document_id creates valid IDs", {
  id <- generate_document_id()
  expect_type(id, "character")
  expect_true(nchar(id) %in% 27:28)
  # Should be decodable to 16 bytes
  bytes <- secretbase::base58dec(id, convert = FALSE)
  expect_length(bytes, 16L)
})

test_that("amsync_server with TLS creates https URL", {
  server <- amsync_server(
    port = 0,
    tls = nanonext::write_cert()$server,
    data_dir = tempdir()
  )
  on.exit(stop_server(server))

  expect_true(grepl("^https://", server$url))
})

test_that("amsync_server without TLS creates http URL", {
  server <- amsync_server(port = 0, data_dir = tempdir())
  on.exit(stop_server(server))

  expect_true(grepl("^http://", server$url))
})
