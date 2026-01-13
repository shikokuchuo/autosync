test_that("save_document writes file to disk", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  state <- new.env(hash = TRUE)
  state$data_dir <- data_dir
  state$documents <- new.env(hash = TRUE)

  doc <- automerge::am_create()
  doc_id <- "testDoc123"

  autosync:::save_document(state, doc_id, doc)

  expected_path <- file.path(data_dir, paste0(doc_id, ".automerge"))
  expect_true(file.exists(expected_path))
  expect_true(file.info(expected_path)$size > 0)
})

test_that("load_all_documents loads existing documents", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  # Create and save a document manually
  doc1 <- automerge::am_create()
  doc1_id <- "loadTest1"
  writeBin(automerge::am_save(doc1), file.path(data_dir, paste0(doc1_id, ".automerge")))

  doc2 <- automerge::am_create()
  doc2_id <- "loadTest2"
  writeBin(automerge::am_save(doc2), file.path(data_dir, paste0(doc2_id, ".automerge")))

  # Create state with empty documents env
  state <- new.env(hash = TRUE)
  state$data_dir <- data_dir
  state$documents <- new.env(hash = TRUE)

  # Load documents
  autosync:::load_all_documents(state)

  # Verify both documents were loaded
  expect_true(exists(doc1_id, envir = state$documents))
  expect_true(exists(doc2_id, envir = state$documents))
  expect_true(inherits(state$documents[[doc1_id]], "am_doc"))
  expect_true(inherits(state$documents[[doc2_id]], "am_doc"))
})

test_that("load_all_documents handles empty directory", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  state <- new.env(hash = TRUE)
  state$data_dir <- data_dir
  state$documents <- new.env(hash = TRUE)

  expect_no_error(autosync:::load_all_documents(state))
  expect_length(ls(state$documents), 0)
})

test_that("load_all_documents handles corrupted files gracefully", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  # Write invalid data
  writeBin(as.raw(c(0xFF, 0xFF, 0xFF)), file.path(data_dir, "corrupt.automerge"))

  state <- new.env(hash = TRUE)
  state$data_dir <- data_dir
  state$documents <- new.env(hash = TRUE)

  # Should warn but not error
  expect_warning(autosync:::load_all_documents(state), "Failed to load document")
  expect_length(ls(state$documents), 0)
})

test_that("save_document and load_all_documents roundtrip", {
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  # Create state and save a document
  state1 <- new.env(hash = TRUE)
  state1$data_dir <- data_dir
  state1$documents <- new.env(hash = TRUE)

  doc <- automerge::am_create()
  doc_id <- "roundtripTest"
  autosync:::save_document(state1, doc_id, doc)

  # Create new state and load
  state2 <- new.env(hash = TRUE)
  state2$data_dir <- data_dir
  state2$documents <- new.env(hash = TRUE)

  autosync:::load_all_documents(state2)

  expect_true(exists(doc_id, envir = state2$documents))
})
