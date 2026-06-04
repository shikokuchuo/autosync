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
  on.exit(proj$close(), add = TRUE)

  expect_s3_class(proj, "amsync_project")
  expect_equal(proj$paths(), c("/charlie/index.qmd", "/notes/todo.md"))
  expect_equal(proj$doc_id("/charlie/index.qmd"), id1)
  expect_equal(proj$doc_id("/notes/todo.md"), id2)
})

test_that("amsync_project opens files over a single reused connection", {
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
  on.exit(proj$close(), add = TRUE)

  conns_before <- length(ls(attr(server, "sync")$connections))
  h1 <- proj$open("/charlie/index.qmd")
  h2 <- proj$open("/notes/todo.md")
  for (i in seq_len(10)) later::run_now(0.05)

  # Browsing files reuses the project's connection rather than dialing again.
  expect_equal(length(ls(attr(server, "sync")$connections)), conns_before)
  expect_identical(h1$stream, proj$conn$stream)
  expect_identical(h2$stream, proj$conn$stream)
  expect_equal(h1$doc_id, id1)
  expect_equal(h2$doc_id, id2)
})

test_that("amsync_project closes the connection when the project fails to open", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  # A server that never auto-creates documents reports the project unavailable,
  # so open_doc() throws and amsync_project() must tear the connection down.
  server <- amsync_server(data_dir = data_dir, auto_create_docs = FALSE)
  server$start()
  on.exit(server$close(), add = TRUE)

  expect_error(
    suppressWarnings(amsync_project(server$url, generate_document_id())),
    "not available"
  )
})

test_that("amsync_project errors when the files key is not a map", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  # `files` is a string rather than a map.
  proj_id <- create_document(server)
  pdoc <- get_document(server, proj_id)
  automerge::am_put(pdoc, automerge::AM_ROOT, "files", "not a map")

  expect_error(
    amsync_project(server$url, proj_id),
    "is not a map"
  )
})

test_that("amsync_project$doc_id errors when an entry is not a text object", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  proj_id <- create_document(server)
  pdoc <- get_document(server, proj_id)
  pdoc[["files"]] <- automerge::am_map()
  files <- pdoc[["files"]]
  # A files entry that is a scalar rather than a text object.
  files[["/bad"]] <- 42L

  proj <- amsync_project(server$url, proj_id)
  on.exit(proj$close(), add = TRUE)

  expect_error(proj$doc_id("/bad"), "not a text object")
})

test_that("amsync_project$refresh re-resolves the file tree", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  id1 <- make_file_doc(server, "x")
  proj_id <- make_project_doc(server, list("/a.md" = id1))
  proj <- amsync_project(server$url, proj_id)
  on.exit(proj$close(), add = TRUE)

  # Refresh settles pending sync and re-resolves the map, returning the project.
  expect_identical(proj$refresh(), proj)
  expect_equal(proj$paths(), "/a.md")
})

test_that("file_ext_dot returns a dotted extension or .txt", {
  expect_equal(autosync:::file_ext_dot("/a/b.md"), ".md")
  expect_equal(autosync:::file_ext_dot("/notes/index.qmd"), ".qmd")
  expect_equal(autosync:::file_ext_dot("README"), ".txt")
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
  on.exit(proj$close(), add = TRUE)

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
  on.exit(proj$close(), add = TRUE)

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
