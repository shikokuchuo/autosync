test_that("amsync_app errors in a non-interactive session", {
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_error(amsync_app(), "requires an interactive session")
})

test_that("amsync_app errors when shiny or bslib is missing", {
  local_mocked_bindings(is_interactive = function() TRUE)
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(amsync_app(), "requires the 'shiny' and 'bslib' packages")
})

test_that("amsync_app builds the app and launches it as a gadget", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  # Mock the interactive check and the gadget launcher so the happy path runs
  # without opening a window; capture the built app instead.
  launched <- NULL
  local_mocked_bindings(is_interactive = function() TRUE)
  local_mocked_bindings(
    runGadget = function(app, ...) {
      launched <<- app
      NULL
    },
    .package = "shiny"
  )

  expect_null(amsync_app("wss://x/ws", proj_id = "DOC123"))
  expect_s3_class(launched, "shiny.appobj")
})

test_that("connect_screen_ui exposes the URL, project, auth, and connect inputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  html <- as.character(connect_screen_ui("wss://x/ws", "DOC123"))

  # The two prefillable fields carry the supplied values.
  expect_match(html, 'id="url"', fixed = TRUE)
  expect_match(html, 'value="wss://x/ws"', fixed = TRUE)
  expect_match(html, 'id="proj_id"', fixed = TRUE)
  expect_match(html, 'value="DOC123"', fixed = TRUE)

  # Auth, advanced fields, and the connect action are all present.
  expect_match(html, 'id="authenticate"', fixed = TRUE)
  expect_match(html, 'id="auth_status"', fixed = TRUE)
  expect_match(html, 'id="client_id"', fixed = TRUE)
  expect_match(html, 'id="client_secret"', fixed = TRUE)
  expect_match(html, 'id="issuer"', fixed = TRUE)
  expect_match(html, 'id="connect"', fixed = TRUE)
  expect_match(html, 'id="exit"', fixed = TRUE)
})

test_that("closed_screen_ui shows a message to close the window", {
  skip_if_not_installed("shiny")
  html <- as.character(closed_screen_ui())
  expect_match(html, "Session ended", fixed = TRUE)
  expect_match(html, "close this window", fixed = TRUE)
})

test_that("Exit shows the closing screen and schedules the app to stop", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  # Stub the delayed stop so the scheduled stopApp() doesn't leak into the
  # shared event loop and disturb other tests; just record that it was queued.
  scheduled <- FALSE
  local_mocked_bindings(
    later = function(func, delay = 0, ...) {
      scheduled <<- TRUE
      invisible()
    }
  )

  app <- build_amsync_app("", "", NULL, NULL, 5000L, "files", 300L)
  shiny::testServer(app, {
    expect_equal(rv$view, "connect")
    session$setInputs(exit = 1)
    expect_equal(rv$view, "closed")
    expect_true(scheduled)
  })
})

test_that("a token passed to the app starts it signed in", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  app <- build_amsync_app("", "", "jwt.tok.en", NULL, 5000L, "files", 300L)
  shiny::testServer(app, {
    expect_true(rv$authed)
    expect_equal(st$token, "jwt.tok.en")
  })
})

test_that("the connect screen renders with the prefilled server URL", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  # Regression: the `server` argument must not be shadowed by the app's server
  # function. Rendering output$screen on the connect view runs
  # connect_screen_ui(server, ...) and would error ("cannot coerce type
  # 'closure'") if `server` resolved to the server function instead of the URL.
  app <- build_amsync_app("wss://x/ws", "DOC123", NULL, NULL, 5000L, "files", 300L)
  shiny::testServer(app, {
    html <- paste(unlist(output$screen), collapse = " ")
    expect_match(html, "Connect to a project", fixed = TRUE)
    expect_match(html, "wss://x/ws", fixed = TRUE)
  })
})

test_that("amsync_app rejects a malformed token", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  # Token validation runs after the interactive + package checks, so mock the
  # session as interactive to reach it.
  local_mocked_bindings(is_interactive = function() TRUE)
  expect_error(amsync_app(token = 123), "single non-empty string")
  expect_error(amsync_app(token = c("a", "b")), "single non-empty string")
  expect_error(amsync_app(token = ""), "single non-empty string")
})

test_that("browse_screen_ui lays out the tree container, actions, and editor", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  html <- as.character(browse_screen_ui())

  expect_match(html, 'id="amsync-filetree"', fixed = TRUE)
  expect_match(html, 'id="filetree"', fixed = TRUE)
  expect_match(html, 'id="refresh"', fixed = TRUE)
  expect_match(html, 'id="disconnect"', fixed = TRUE)
  expect_match(html, 'id="editor"', fixed = TRUE)
  # File-row clicks are wired to input$file via the delegated handler.
  expect_match(html, "Shiny.setInputValue('file'", fixed = TRUE)
})

test_that("build_file_tree_ui renders a collapsible tree with original paths", {
  skip_if_not_installed("shiny")

  html <- as.character(build_file_tree_ui(
    c("/charlie/index.qmd", "/charlie/about.qmd", "/notes/todo.md"),
    selected = "/notes/todo.md"
  ))

  # Folders become <details>/<summary>; nested files keep their ORIGINAL path.
  expect_match(html, "<details", fixed = TRUE)
  expect_match(html, "<summary>charlie</summary>", fixed = TRUE)
  expect_match(html, 'data-path="/charlie/index.qmd"', fixed = TRUE)
  expect_match(html, 'data-path="/notes/todo.md"', fixed = TRUE)
  # Leaf labels are the file name, not the full path.
  expect_match(html, ">index.qmd<", fixed = TRUE)
  # The open file is marked active.
  expect_match(html, 'class="amsync-file active"', fixed = TRUE)
})

test_that("build_file_tree_ui shows a placeholder when there are no files", {
  skip_if_not_installed("shiny")
  expect_match(as.character(build_file_tree_ui(character(0))), "No files")
})

test_that("editor_card_ui shows the path and embeds the streaming shim", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  html <- as.character(editor_card_ui("/notes.md", "hello", ".md", 450L))

  expect_match(html, 'id="content"', fixed = TRUE)
  expect_match(html, "/notes.md", fixed = TRUE)
  # The debounce flows through to the editor-streaming JavaScript.
  expect_match(html, "var DEBOUNCE = 450;", fixed = TRUE)
})

test_that("editor_placeholder_ui prompts the user to pick a file", {
  skip_if_not_installed("shiny")
  expect_match(as.character(editor_placeholder_ui()), "Select a file")
})

test_that("auth_status_ui reflects the sign-in state", {
  skip_if_not_installed("shiny")
  expect_match(as.character(auth_status_ui(TRUE)), "signed in")
  expect_match(as.character(auth_status_ui(TRUE)), "text-success", fixed = TRUE)
  expect_match(as.character(auth_status_ui(FALSE)), "not signed in")
  expect_match(as.character(auth_status_ui(FALSE)), "text-muted", fixed = TRUE)
})

test_that("the app connects, browses, and edits over a live server", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  drain_later()

  data_dir <- tempfile()
  dir.create(data_dir)
  on.exit(unlink(data_dir, recursive = TRUE))

  server <- amsync_server(data_dir = data_dir)
  server$start()
  on.exit(server$close(), add = TRUE)

  # Seed a project document whose `files` map holds one text file.
  fid <- create_document(server)
  fdoc <- get_document(server, fid)
  fdoc[["text"]] <- automerge::am_text("hello world")
  pid <- create_document(server)
  pdoc <- get_document(server, pid)
  pdoc[["files"]] <- automerge::am_map()
  files <- pdoc[["files"]]
  files[["/notes.md"]] <- automerge::am_text(fid)

  app <- build_amsync_app(
    server = server$url,
    proj_id = pid,
    token = NULL,
    tls = NULL,
    timeout = 5000L,
    files_key = "files",
    debounce = 300L
  )

  # testServer evaluates the expr with the mock session's reactives (input,
  # output, session, and the server function's locals st/rv) layered over this
  # test's environment, so the `server`/`pid` test locals are reachable here.
  shiny::testServer(app, {
    expect_equal(rv$view, "connect")
    expect_false(rv$authed)

    # Connecting builds the project (its run_now() handshake runs inside the
    # observer) and switches to the browse screen with the file tree loaded.
    session$setInputs(url = server$url, proj_id = pid)
    session$setInputs(connect = 1)
    expect_equal(rv$view, "browse")
    expect_equal(rv$paths, "/notes.md")
    expect_s3_class(st$proj, "amsync_project")

    # Opening the file loads its document and content into the editor.
    session$setInputs(file = "/notes.md")
    expect_equal(rv$selected, "/notes.md")
    expect_equal(st$base, "hello world")
    expect_s3_class(st$doc, "amsync_doc")

    # Typing in the editor writes the minimal diff into the live document.
    session$setInputs(content = "hello brave world")
    expect_equal(
      automerge::am_text_content(st$doc$doc[["text"]]),
      "hello brave world"
    )

    # Disconnecting tears down the connection and returns to the connect form.
    session$setInputs(disconnect = 1)
    expect_equal(rv$view, "connect")
    expect_null(st$proj)
  })
})

test_that("the Authenticate button signs in via amsync_token()", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  local_mocked_bindings(amsync_token = function(...) "fresh.jwt")
  app <- build_amsync_app("", "", NULL, NULL, 5000L, "files", 300L)
  shiny::testServer(app, {
    expect_false(rv$authed)
    # A non-empty issuer is used as-is (exercises the issuer branch).
    session$setInputs(client_id = "cid", client_secret = "sec", issuer = "https://issuer")
    session$setInputs(authenticate = 1)
    expect_true(rv$authed)
    expect_equal(st$token, "fresh.jwt")
  })
})

test_that("a failed Authenticate leaves the session signed out", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  local_mocked_bindings(amsync_token = function(...) stop("denied"))
  app <- build_amsync_app("", "", NULL, NULL, 5000L, "files", 300L)
  shiny::testServer(app, {
    # Blank issuer falls back to oidc_issuer() (the other branch).
    session$setInputs(issuer = "")
    session$setInputs(authenticate = 1)
    expect_false(rv$authed)
    expect_null(st$token)
  })
})

test_that("Connect warns and stays put when the URL or project ID is blank", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  app <- build_amsync_app("", "", NULL, NULL, 5000L, "files", 300L)
  shiny::testServer(app, {
    session$setInputs(url = "", proj_id = "")
    session$setInputs(connect = 1)
    expect_equal(rv$view, "connect")
    expect_null(st$proj)
  })
})

test_that("a failed Connect stays on the connect screen", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  local_mocked_bindings(amsync_project = function(...) stop("cannot connect"))
  app <- build_amsync_app("", "", NULL, NULL, 5000L, "files", 300L)
  shiny::testServer(app, {
    session$setInputs(url = "wss://x/ws", proj_id = "DOC123")
    session$setInputs(connect = 1)
    expect_equal(rv$view, "connect")
    expect_null(st$proj)
  })
})

test_that("opening a file ignores a blank path and reports open errors", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  app <- build_amsync_app("", "", NULL, NULL, 5000L, "files", 300L)
  shiny::testServer(app, {
    # The observer is ignoreInit, so the first input change is swallowed; prime
    # it before the cases we want to observe.
    session$setInputs(file = "/prime.md")

    # A blank path is ignored.
    session$setInputs(file = "")
    expect_null(rv$selected)

    # A project whose open() fails: the error is caught and no file opens.
    st$proj <- list(open = function(path) stop("boom"))
    session$setInputs(file = "/notes.md")
    expect_null(rv$selected)
    expect_null(st$doc)
  })
})

test_that("Refresh re-resolves the tree and drops a vanished selection", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  app <- build_amsync_app("", "", NULL, NULL, 5000L, "files", 300L)
  shiny::testServer(app, {
    # No project held: refresh is a quiet no-op.
    session$setInputs(refresh = 1)
    expect_equal(rv$paths, character(0))

    # With a project, the tree re-resolves and a now-missing selection clears.
    refreshed <- FALSE
    st$proj <- list(
      refresh = function() refreshed <<- TRUE,
      paths = function() c("/a.md", "/b.md")
    )
    rv$selected <- "/gone.md"
    session$setInputs(refresh = 2)
    expect_true(refreshed)
    expect_equal(rv$paths, c("/a.md", "/b.md"))
    expect_null(rv$selected)
  })
})

test_that("build_file_tree_ui skips path entries with no real components", {
  skip_if_not_installed("shiny")

  # A path of only separators contributes no nodes (the skipped-entry branch).
  html <- as.character(build_file_tree_ui("/"))
  expect_false(grepl("data-path", html, fixed = TRUE))
})

test_that("cleanup_project closes the connection and clears state", {
  closed <- FALSE
  st <- new.env(parent = emptyenv())
  st$proj <- list(close = function() closed <<- TRUE)
  st$doc <- "handle"

  cleanup_project(st)

  expect_true(closed)
  expect_null(st$proj)
  expect_null(st$doc)

  # Idempotent: a second call (no project held) is a quiet no-op.
  expect_no_error(cleanup_project(st))
})
