# Helper: seed a server document with a text object at `key`
seed_text_doc <- function(server, content, key = "text") {
  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  doc[[key]] <- automerge::am_text(content)
  doc_id
}

# Helper: a standalone (server-less) Automerge document with a text object,
# for exercising the sync helpers without a connection.
local_text_doc <- function(content, key = "text") {
  doc <- automerge::am_create()
  doc[[key]] <- automerge::am_text(content)
  doc
}

test_that("sync_editor_to_doc applies edits and pushes to the server", {
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

  shown <- sync_editor_to_doc(
    doc$doc[["text"]],
    "hello brave world",
    "hello world",
    doc$push
  )

  expect_equal(shown, "hello brave world")
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

test_that("sync_editor_to_doc writes the minimal diff and is a no-op unchanged", {
  doc <- local_text_doc("unchanged")
  pushes <- 0L
  push <- function() pushes <<- pushes + 1L

  # Unchanged content: no write, no push.
  shown <- sync_editor_to_doc(doc[["text"]], "unchanged", "unchanged", push)
  expect_equal(shown, "unchanged")
  expect_equal(pushes, 0L)
  expect_equal(automerge::am_text_content(doc[["text"]]), "unchanged")

  # A real edit: written and pushed once.
  shown <- sync_editor_to_doc(doc[["text"]], "changed", "unchanged", push)
  expect_equal(shown, "changed")
  expect_equal(pushes, 1L)
  expect_equal(automerge::am_text_content(doc[["text"]]), "changed")
})

test_that("sync_editor_to_doc preserves the original trailing-newline state", {
  doc <- local_text_doc("line one")
  push <- function() invisible()

  # Original has no trailing newline; editor appends one -> stripped.
  shown <- sync_editor_to_doc(
    doc[["text"]],
    "line one\nline two\n",
    "line one",
    push
  )
  expect_equal(shown, "line one\nline two")
  expect_equal(automerge::am_text_content(doc[["text"]]), "line one\nline two")
})

test_that("poll_doc_to_editor reports remote changes to reflect in the editor", {
  doc <- local_text_doc("hello world")

  # Document matches what the editor shows: nothing to reflect back.
  expect_null(poll_doc_to_editor(doc[["text"]], "hello world"))

  # A change arrives on the live document (e.g. from a remote peer).
  automerge::am_text_splice(doc[["text"]], 0L, 0L, ">> ")
  expect_equal(
    poll_doc_to_editor(doc[["text"]], "hello world"),
    ">> hello world"
  )
})

test_that("the editor and document converge without echoing", {
  doc <- local_text_doc("hello world")
  pushes <- 0L
  push <- function() pushes <<- pushes + 1L

  # User types: the outgoing sync writes the diff and pushes once.
  shown <- sync_editor_to_doc(
    doc[["text"]],
    "hello there world",
    "hello world",
    push
  )
  expect_equal(pushes, 1L)

  # The poll sees the document matches the editor -> nothing to reflect back.
  expect_null(poll_doc_to_editor(doc[["text"]], shown))

  # A remote edit arrives; the poll reports it and the editor adopts it.
  automerge::am_text_splice(doc[["text"]], 0L, 0L, ">> ")
  current <- poll_doc_to_editor(doc[["text"]], shown)
  expect_equal(current, ">> hello there world")
  shown <- current

  # Re-sending the adopted content is a no-op: no echo push, no spurious diff.
  shown <- sync_editor_to_doc(doc[["text"]], shown, "hello world", push)
  expect_equal(pushes, 1L)
  expect_equal(automerge::am_text_content(doc[["text"]]), ">> hello there world")
})

test_that("editor_stream_js embeds the debounce and streams via the binding", {
  js <- editor_stream_js(450)
  expect_match(js, "var DEBOUNCE = 450;", fixed = TRUE)
  expect_match(js, "el.prismEditor.on('update'", fixed = TRUE)
  expect_match(js, "el.onChangeCallback(false)", fixed = TRUE)

  # Coerced to an integer literal (no decimals leak into the JS).
  expect_match(editor_stream_js(300L), "var DEBOUNCE = 300;", fixed = TRUE)
})

test_that("doc$edit errors when the target is not a text object", {
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

  # The handle exposes the editor as a method.
  expect_true(is.function(handle$edit))
  expect_error(
    handle$edit(at = "num"),
    "not a text object"
  )
})

test_that("edit_doc validates its arguments", {
  fake <- structure(new.env(parent = emptyenv()), class = "amsync_doc")
  fake$active <- FALSE
  expect_error(edit_doc(fake), "not active")

  # An active handle reaches the `at` / `debounce` checks (which error before
  # the editor would launch).
  fake$active <- TRUE
  fake$doc <- local_text_doc("hi")
  expect_error(edit_doc(fake, at = character()), "non-empty character path")
  expect_error(edit_doc(fake, debounce = -1), "non-negative")
  expect_error(edit_doc(fake, debounce = c(1, 2)), "single non-negative")
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
