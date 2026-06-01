# Helper: create a file document with a text object, return its doc-id
make_file_doc <- function(server, content) {
  id <- create_document(server)
  doc <- get_document(server, id)
  doc[["text"]] <- automerge::am_text(content)
  id
}

# Helper: create a project document whose `files` map points paths -> doc-ids.
# `files` is a named list (path = doc_id).
make_project_doc <- function(server, files, files_key = "files") {
  pid <- create_document(server)
  pdoc <- get_document(server, pid)
  pdoc[[files_key]] <- automerge::am_map()
  m <- pdoc[[files_key]]
  for (path in names(files)) {
    m[[path]] <- automerge::am_text(files[[path]])
  }
  pid
}

test_that("amsync_project lists paths and resolves doc-ids", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  id1 <- make_file_doc(server, "# Index")
  id2 <- make_file_doc(server, "todo items")
  proj_id <- make_project_doc(server, list(
    "/charlie/index.qmd" = id1,
    "/notes/todo.md" = id2
  ))

  proj <- amsync_project(server$url, proj_id)

  expect_s3_class(proj, "amsync_project")
  expect_equal(proj$paths(), c("/charlie/index.qmd", "/notes/todo.md"))
  expect_equal(proj$doc_id("/charlie/index.qmd"), id1)
  expect_equal(proj$doc_id("/notes/todo.md"), id2)
})

test_that("amsync_project$edit opens the right file and infers the extension", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  id1 <- make_file_doc(server, "# Index")
  id2 <- make_file_doc(server, "todo")
  proj_id <- make_project_doc(server, list(
    "/charlie/index.qmd" = id1,
    "/notes/todo.md" = id2
  ))

  proj <- amsync_project(server$url, proj_id)

  captured <- new.env()
  local_mocked_bindings(
    amsync_edit = function(client, at = "text", editor = NULL, ext = NULL) {
      captured$ext <- ext
      captured$doc_id <- client$doc_id
      invisible(client)
    }
  )

  proj$edit("/charlie/index.qmd")
  expect_equal(captured$ext, ".qmd")
  expect_equal(captured$doc_id, id1)

  proj$edit("/notes/todo.md")
  expect_equal(captured$ext, ".md")
  expect_equal(captured$doc_id, id2)
})

test_that("amsync_project$edit performs a real edit and pushes", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  id1 <- make_file_doc(server, "old")
  proj_id <- make_project_doc(server, list("/a/b.md" = id1))

  proj <- amsync_project(server$url, proj_id)

  local_mocked_bindings(
    launch_editor = function(path, editor = NULL) {
      writeBin(charToRaw("new content"), path)
      invisible()
    }
  )

  proj$edit("/a/b.md")
  for (i in seq_len(20)) later::run_now(0.1)

  server_file <- get_document(server, id1)
  expect_equal(
    automerge::am_text_content(server_file[["text"]]),
    "new content"
  )
})

test_that("amsync_project errors on a missing files map", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  # A document with no `files` map.
  proj_id <- create_document(server)
  pdoc <- get_document(server, proj_id)
  automerge::am_put(pdoc, automerge::AM_ROOT, "title", "no files here")

  expect_error(
    amsync_project(server$url, proj_id),
    "no `files` map"
  )
})

test_that("amsync_project$doc_id errors on an unknown path", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  id1 <- make_file_doc(server, "x")
  proj_id <- make_project_doc(server, list("/a/b.md" = id1))

  proj <- amsync_project(server$url, proj_id)

  expect_error(
    proj$doc_id("/does/not/exist"),
    "Unknown path"
  )
})

test_that("print.amsync_project shows the tree and metadata", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  id1 <- make_file_doc(server, "x")
  proj_id <- make_project_doc(server, list("/a/b.md" = id1))
  proj <- amsync_project(server$url, proj_id)

  out <- capture.output(print(proj))
  expect_true(any(grepl("Automerge Project", out)))
  expect_true(any(grepl(proj_id, out, fixed = TRUE)))
  expect_true(any(grepl("b.md", out)))
})

test_that("format_file_tree renders a nested tree", {
  expect_snapshot(
    cat(autosync:::format_file_tree(c(
      "/charlie/data.csv",
      "/charlie/index.qmd",
      "/charlie/deep/notes.txt",
      "/notes/todo.md",
      "/readme.md"
    )))
  )
})

test_that("format_file_tree handles the empty case", {
  expect_equal(autosync:::format_file_tree(character(0)), "/\n")
})
