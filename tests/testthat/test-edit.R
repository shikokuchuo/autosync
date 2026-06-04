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

# Helper: a fake amsync_doc handle backed by a standalone text document, for
# exercising the editor without a live connection. `$push` just counts calls.
fake_doc_handle <- function(content, key = "text", active = TRUE) {
  handle <- structure(new.env(parent = emptyenv()), class = "amsync_doc")
  handle$active <- active
  handle$doc <- local_text_doc(content, key)
  handle$push_count <- 0L
  handle$push <- function() handle$push_count <- handle$push_count + 1L
  handle
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

test_that("navigate_to_text errors when the path has no object", {
  expect_error(
    navigate_to_text(local_text_doc("x"), "missing"),
    "No object found at path: missing"
  )
  # A nested miss (through a real map) reports the full path.
  doc <- automerge::am_create()
  doc[["folder"]] <- automerge::am_map()
  expect_error(
    navigate_to_text(doc, c("folder", "missing")),
    "No object found at path: folder/missing"
  )
})

test_that("ext_to_language covers every mapped language group", {
  # One representative extension per distinct language mapping.
  expect_equal(ext_to_language("jl"), "julia")
  expect_equal(ext_to_language("sql"), "sql")
  expect_equal(ext_to_language("js"), "javascript")
  expect_equal(ext_to_language("ts"), "typescript")
  expect_equal(ext_to_language("html"), "html")
  expect_equal(ext_to_language("css"), "css")
  expect_equal(ext_to_language("scss"), "scss")
  expect_equal(ext_to_language("sass"), "sass")
  expect_equal(ext_to_language("json"), "json")
  expect_equal(ext_to_language("xml"), "xml")
  expect_equal(ext_to_language("toml"), "toml")
  expect_equal(ext_to_language("ini"), "ini")
  expect_equal(ext_to_language("sh"), "bash")
  expect_equal(ext_to_language("dockerfile"), "docker")
  expect_equal(ext_to_language("tex"), "latex")
  expect_equal(ext_to_language("rs"), "rust")
  expect_equal(ext_to_language("diff"), "diff")
})

test_that("install_editor_sync is a no-op while no document is open", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  st <- new.env(parent = emptyenv())
  st$doc <- NULL
  st$at <- "text"
  st$base <- ""
  st$shown <- ""
  # A large poll interval keeps the scheduled invalidateLater from re-firing,
  # so a single flushReact() runs the incoming observer exactly once.
  app <- shiny::shinyApp(
    shiny::fluidPage(),
    function(input, output, session) install_editor_sync(input, st, poll_ms = 1e9)
  )

  shiny::testServer(app, {
    session$flushReact() # incoming poll: no document -> early return
    session$setInputs(content = "typed") # outgoing edit: no document -> early return
    expect_equal(st$shown, "") # nothing written
  })
})

test_that("install_editor_sync reflects a remote change back into the editor", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  handle <- fake_doc_handle("hello world")
  st <- new.env(parent = emptyenv())
  st$doc <- handle
  st$at <- "text"
  st$base <- "hello world"
  st$shown <- "hello world"
  # A remote peer edits the live document before the editor polls.
  automerge::am_text_splice(handle$doc[["text"]], 0L, 0L, ">> ")

  app <- shiny::shinyApp(
    shiny::fluidPage(),
    function(input, output, session) install_editor_sync(input, st, poll_ms = 1e9)
  )

  shiny::testServer(app, {
    session$flushReact() # incoming poll: current differs from shown -> adopt it
    expect_equal(st$shown, ">> hello world")
  })
})

test_that("the editor errors when shiny or bslib is missing", {
  handle <- fake_doc_handle("hello world")
  # The target validates first, then the editor checks for its UI dependencies.
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(
    edit_doc(handle, at = "text"),
    "requires the 'shiny' and 'bslib' packages"
  )
})

test_that("edit_doc launches the live editor and reports the final content", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  handle <- fake_doc_handle("hello world")

  # Capture the gadget the editor would launch rather than running it, and
  # report a known final content so the closing message is deterministic.
  captured <- NULL
  local_mocked_bindings(
    runGadget = function(app, ...) {
      captured <<- app
      "done!"
    },
    .package = "shiny"
  )

  expect_message(
    expect_identical(edit_doc(handle, at = "text"), handle),
    "Closed editor for text \\(5 chars\\)\\."
  )
  expect_s3_class(captured, "shiny.appobj")

  # Drive the captured gadget's server to exercise its session logic.
  shiny::testServer(captured, {
    session$flushReact() # incoming poll observer runs once (no remote change)

    # Typing streams the minimal diff into the live document and pushes.
    session$setInputs(content = "hello brave world")
    expect_equal(
      automerge::am_text_content(handle$doc[["text"]]),
      "hello brave world"
    )
    expect_gt(handle$push_count, 0L)

    # Close stops the gadget once; a second close is an idempotent no-op.
    session$setInputs(close = 1)
    session$setInputs(close = 2)
  })
})
