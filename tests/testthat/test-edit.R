# Helper: seed a server document with a text object at `key`
seed_text_doc <- function(server, content, key = "text") {
  doc_id <- create_document(server)
  doc <- get_document(server, doc_id)
  doc[[key]] <- automerge::am_text(content)
  doc_id
}

# Helper: a launch_editor mock that overwrites the tempfile with `new_content`
# (optionally running a side effect first, e.g. a concurrent remote edit).
mock_editor_writes <- function(new_content, side_effect = NULL) {
  function(path, editor = NULL) {
    if (!is.null(side_effect)) side_effect()
    writeBin(charToRaw(enc2utf8(new_content)), path)
    invisible()
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
  client <- amsync_client(server$url, doc_id)
  on.exit(client$close(), add = TRUE)

  local_mocked_bindings(launch_editor = mock_editor_writes("hello brave world"))

  expect_message(
    amsync_edit(client, at = "text"),
    "Updated text: 11 -> 17 chars; pushed."
  )

  expect_equal(
    automerge::am_text_content(client$doc[["text"]]),
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
  client <- amsync_client(server$url, doc_id)
  on.exit(client$close(), add = TRUE)

  # While "editing", a remote edit lands on the live doc (the fork was taken
  # at "hello world"). The merge must keep both edits.
  remote_edit <- function() {
    automerge::am_text_splice(client$doc[["text"]], 0L, 0L, ">> ")
  }
  local_mocked_bindings(
    launch_editor = mock_editor_writes("hello brave world", remote_edit)
  )

  amsync_edit(client, at = "text")

  result <- automerge::am_text_content(client$doc[["text"]])
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
  client <- amsync_client(server$url, doc_id)
  on.exit(client$close(), add = TRUE)

  # Editor leaves the file exactly as written.
  local_mocked_bindings(launch_editor = function(path, editor = NULL) invisible())

  expect_message(
    res <- amsync_edit(client, at = "text"),
    "No changes."
  )
  expect_identical(res, client)
  expect_equal(
    automerge::am_text_content(client$doc[["text"]]),
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
  client <- amsync_client(server$url, doc_id)
  on.exit(client$close(), add = TRUE)

  local_mocked_bindings(launch_editor = mock_editor_writes("line one\nline two\n"))
  amsync_edit(client, at = "text")

  expect_equal(
    automerge::am_text_content(client$doc[["text"]]),
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

  client <- amsync_client(server$url, doc_id)
  on.exit(client$close(), add = TRUE)

  expect_error(
    amsync_edit(client, at = "num"),
    "not a text object"
  )
})

test_that("amsync_edit validates its client argument", {
  expect_error(amsync_edit(list()), "must be an `amsync_client`")

  fake <- structure(new.env(parent = emptyenv()), class = "amsync_client")
  fake$active <- FALSE
  expect_error(amsync_edit(fake), "not active")
})

test_that("resolve_editor follows the documented precedence", {
  old_opt <- options(editor = NULL)
  old_visual <- Sys.getenv("VISUAL", unset = NA)
  old_editor <- Sys.getenv("EDITOR", unset = NA)
  on.exit({
    options(old_opt)
    if (is.na(old_visual)) Sys.unsetenv("VISUAL") else Sys.setenv(VISUAL = old_visual)
    if (is.na(old_editor)) Sys.unsetenv("EDITOR") else Sys.setenv(EDITOR = old_editor)
  })

  Sys.setenv(VISUAL = "visual-ed", EDITOR = "editor-ed")
  options(editor = "option-ed")

  # explicit arg wins over everything
  expect_equal(resolve_editor("arg-ed"), "arg-ed")
  # getOption("editor") wins over env vars
  expect_equal(resolve_editor(NULL), "option-ed")

  # without the option, $VISUAL beats $EDITOR
  options(editor = NULL)
  expect_equal(resolve_editor(NULL), "visual-ed")

  # without $VISUAL, falls back to $EDITOR
  Sys.unsetenv("VISUAL")
  expect_equal(resolve_editor(NULL), "editor-ed")
})

test_that("launch_editor surfaces a non-zero editor exit status", {
  skip_on_os("windows")
  tmp <- tempfile()
  file.create(tmp)
  on.exit(unlink(tmp))

  # `false` exits 1 without touching the file.
  expect_error(
    launch_editor(tmp, editor = "false"),
    "exited with status"
  )
})

test_that("match_trailing_newline mirrors the base string", {
  expect_equal(match_trailing_newline("a\n", "no-nl"), "a")
  expect_equal(match_trailing_newline("a\n\n", "no-nl"), "a")
  expect_equal(match_trailing_newline("a\n", "has-nl\n"), "a\n")
  expect_equal(match_trailing_newline("a", "no-nl"), "a")
})
