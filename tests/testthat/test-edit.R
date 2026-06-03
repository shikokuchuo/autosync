# Helper: seed a server document with a text object at `key`
seed_text_doc <- function(server, content, key = "text") {
  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  doc[[key]] <- automerge::am_text(content)
  doc_id
}

# Helper: an edit_in_shiny mock that returns `new_content` (optionally running
# a side effect first, e.g. a concurrent remote edit). `NULL` content models a
# cancelled / closed editor.
mock_editor_returns <- function(new_content, side_effect = NULL) {
  function(text, ext = NULL) {
    if (!is.null(side_effect)) side_effect()
    new_content
  }
}

test_that("amsync_edit round-trips edits and pushes to the server", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- seed_text_doc(server, "hello world")
  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)
  doc <- conn$open_doc(doc_id)

  local_mocked_bindings(edit_in_shiny = mock_editor_returns("hello brave world"))

  expect_message(
    amsync_edit(doc, at = "text"),
    "Updated text: 11 -> 17 chars; pushed."
  )

  expect_equal(
    automerge::am_text_content(doc$doc[["text"]]),
    "hello brave world"
  )

  # Let the server apply the pushed sync.
  for (i in seq_len(20)) later::run_now(0.1)
  server_doc <- get_document(server, doc_id)
  expect_equal(
    automerge::am_text_content(server_doc[["text"]]),
    "hello brave world"
  )
})

test_that("amsync_edit preserves a concurrent remote edit", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- seed_text_doc(server, "hello world")
  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)
  doc <- conn$open_doc(doc_id)

  # While "editing", a remote edit lands on the live doc (the fork was taken
  # at "hello world"). The merge must keep both edits.
  remote_edit <- function() {
    automerge::am_text_splice(doc$doc[["text"]], 0L, 0L, ">> ")
  }
  local_mocked_bindings(
    edit_in_shiny = mock_editor_returns("hello brave world", remote_edit)
  )

  amsync_edit(doc, at = "text")

  result <- automerge::am_text_content(doc$doc[["text"]])
  expect_match(result, "brave")
  expect_match(result, ">>", fixed = TRUE)
})

test_that("amsync_edit is a no-op when the content is unchanged", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- seed_text_doc(server, "unchanged")
  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)
  doc <- conn$open_doc(doc_id)

  # Editor returns the content unchanged.
  local_mocked_bindings(edit_in_shiny = function(text, ext = NULL) text)

  expect_message(
    res <- amsync_edit(doc, at = "text"),
    "No changes."
  )
  expect_identical(res, doc)
  expect_equal(
    automerge::am_text_content(doc$doc[["text"]]),
    "unchanged"
  )
})

test_that("amsync_edit is a no-op when the editor is cancelled", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- seed_text_doc(server, "unchanged")
  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)
  doc <- conn$open_doc(doc_id)

  # The editor was closed without saving.
  local_mocked_bindings(edit_in_shiny = mock_editor_returns(NULL))

  expect_message(
    res <- amsync_edit(doc, at = "text"),
    "Edit cancelled."
  )
  expect_identical(res, doc)
  expect_equal(
    automerge::am_text_content(doc$doc[["text"]]),
    "unchanged"
  )
})

test_that("amsync_edit preserves the original trailing-newline state", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  # Original has no trailing newline; editor appends one -> result has none.
  doc_id <- seed_text_doc(server, "line one")
  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)
  doc <- conn$open_doc(doc_id)

  local_mocked_bindings(edit_in_shiny = mock_editor_returns("line one\nline two\n"))
  amsync_edit(doc, at = "text")

  expect_equal(
    automerge::am_text_content(doc$doc[["text"]]),
    "line one\nline two"
  )
})

test_that("amsync_edit errors when the target is not a text object", {
  skip_on_cran()
  drain_later()
  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  doc[["num"]] <- 42L

  conn <- amsync_client(server$url)
  on.exit(conn$close(), add = TRUE)
  handle <- conn$open_doc(doc_id)

  expect_error(
    amsync_edit(handle, at = "num"),
    "not a text object"
  )
})

test_that("amsync_edit validates its doc argument", {
  expect_error(amsync_edit(list()), "must be an `amsync_doc`")

  fake <- structure(new.env(parent = emptyenv()), class = "amsync_doc")
  fake$active <- FALSE
  expect_error(amsync_edit(fake), "not active")
})

test_that("ext_to_language maps extensions to editor languages", {
  # Leading dot optional, case-insensitive.
  expect_equal(ext_to_language(".R"), "r")
  expect_equal(ext_to_language("py"), "python")
  expect_equal(ext_to_language(".qmd"), "markdown")
  expect_equal(ext_to_language(".Rmd"), "markdown")
  expect_equal(ext_to_language("YAML"), "yaml")
  expect_equal(ext_to_language(".cpp"), "cpp")

  # Missing / empty / unknown all fall back to plain.
  expect_equal(ext_to_language(NULL), "plain")
  expect_equal(ext_to_language(""), "plain")
  expect_equal(ext_to_language(".unknown"), "plain")
})

test_that("match_trailing_newline mirrors the base string", {
  expect_equal(match_trailing_newline("a\n", "no-nl"), "a")
  expect_equal(match_trailing_newline("a\n\n", "no-nl"), "a")
  expect_equal(match_trailing_newline("a\n", "has-nl\n"), "a\n")
  expect_equal(match_trailing_newline("a", "no-nl"), "a")
})
