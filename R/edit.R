# Edit a synced Automerge text object in a live Shiny code editor

#' Edit a synced text object in a live Shiny editor
#'
#' Opens a synced Automerge text object in a Shiny app featuring a
#' [bslib::input_code_editor()] component that stays in sync with the
#' collaborative document in both directions:
#'
#' * **Outgoing** -- as you type, the editor's contents are written back into
#'   the live document and pushed to the server, debounced so that a burst of
#'   keystrokes coalesces into one update.
#' * **Incoming** -- when the document's text changes remotely (another peer
#'   edits it), the editor updates automatically to show the merged result.
#'
#' There is no **Save** button: every edit is applied live. Closing the app
#' (the **Close** button or closing the window) simply stops syncing through the
#' editor; the document and its connection are otherwise untouched.
#'
#' @param doc An `amsync_doc` handle (open one with `amsync_client()$open_doc()`)
#'   backed by an active connection.
#' @param at Character path to the text object within the document. A single
#'   string (e.g. `"text"`) addresses a top-level key; a character vector
#'   (e.g. `c("files", "x")`) navigates nested objects with `[[`. Default
#'   `"text"`.
#' @param ext File extension (e.g. `".md"`, with or without the leading dot)
#'   used to pick the editor's syntax-highlighting language. `NULL` (default)
#'   uses plain text.
#' @param debounce Milliseconds to wait after the last keystroke before pushing
#'   the editor's contents to the document. Default 300. Lower values feel more
#'   immediate but push more often; `0` pushes on every change.
#'
#' @return Invisibly returns `doc`.
#'
#' @details
#' Requires the \pkg{shiny} and \pkg{bslib} packages.
#'
#' Edits are applied directly to the live document rather than to a fork.
#' [automerge::am_text_update()] writes only the minimal diff between the
#' editor's contents and the document, so local and remote edits in disjoint
#' regions are preserved. While the app runs the connection keeps syncing, so
#' remote changes land on the live document and the poll loop reflects them
#' back into the editor shortly after they arrive.
#'
#' The editor syncs whole-text snapshots, not granular operations, so it is not
#' a conflict-free collaborative editor: a remote edit that arrives in the brief
#' window between a keystroke and its debounced push can be overwritten by the
#' next push. A small `debounce` narrows this window.
#'
#' The original's trailing-newline state is preserved: if the text did not end
#' in a newline, any trailing newline(s) the editor appends are stripped before
#' the diff is computed.
#'
#' @examplesIf interactive()
#' server <- amsync_server()
#' server$start()
#' doc_id <- create_document(server)
#' sdoc <- get_document(server, doc_id)
#' sdoc$text <- automerge::am_text("edit me")
#'
#' conn <- amsync_client(server$url)
#' doc <- conn$open_doc(doc_id)
#' amsync_edit(doc, at = "text", ext = ".md")
#'
#' conn$close()
#' server$close()
#'
#' @importFrom automerge am_text_content am_text_update
#' @export
amsync_edit <- function(doc, at = "text", ext = NULL, debounce = 300L) {
  if (!inherits(doc, "amsync_doc")) {
    stop("`doc` must be an `amsync_doc` object (see `amsync_client()$open_doc()`)")
  }
  if (!isTRUE(doc$active)) {
    stop("`doc` is not active; reopen it with `$open_doc()`")
  }
  if (!is.character(at) || !length(at) || anyNA(at) || any(!nzchar(at))) {
    stop("`at` must be a non-empty character path")
  }
  if (
    !is.numeric(debounce) || length(debounce) != 1L || is.na(debounce) ||
      debounce < 0
  ) {
    stop("`debounce` must be a single non-negative number of milliseconds")
  }

  # Validate the target is a text object before launching the editor.
  navigate_to_text(doc$doc, at)

  final <- edit_in_shiny(doc, at, doc$push, ext = ext, debounce = debounce)

  message(sprintf(
    "Closed editor for %s (%d chars).",
    paste(at, collapse = "/"),
    nchar(final %||% "", type = "bytes")
  ))
  invisible(doc)
}

#' Navigate a document to a text object via a character path
#'
#' @param doc An Automerge document (or forked document).
#' @param at Character vector path navigated with `[[`.
#'
#' @return The `am_text` object at the path.
#'
#' @noRd
navigate_to_text <- function(doc, at) {
  node <- doc
  for (key in at) {
    node <- node[[key]]
    if (is.null(node)) {
      stop("No object found at path: ", paste(at, collapse = "/"))
    }
  }
  if (!inherits(node, "am_text")) {
    stop(
      "Path ", paste(at, collapse = "/"), " is not a text object (got ",
      paste(class(node), collapse = "/"), ")"
    )
  }
  node
}

#' Write an editor value into the live document
#'
#' Normalises `value`'s trailing-newline state to match `base`, then, if it
#' differs from the document's current content, applies the minimal diff and
#' pushes. Returns the content now agreed between editor and document, which the
#' caller tracks to distinguish its own writes from later remote changes.
#'
#' @param target The live `am_text` object.
#' @param value The editor's current contents.
#' @param base The text the editor opened with (for trailing-newline state).
#' @param push A zero-argument function that pushes local changes to the server.
#'
#' @return The (normalised) content now in the document.
#'
#' @noRd
sync_editor_to_doc <- function(target, value, base, push) {
  value <- match_trailing_newline(enc2utf8(value), base)
  if (!identical(value, am_text_content(target))) {
    am_text_update(target, value)
    push()
  }
  value
}

#' Detect a remote change to reflect into the editor
#'
#' @param target The live `am_text` object.
#' @param shown The content currently reflected in the editor.
#'
#' @return The document's current content if it differs from `shown` (the
#'   editor should be updated to it), otherwise `NULL`.
#'
#' @noRd
poll_doc_to_editor <- function(target, shown) {
  current <- am_text_content(target)
  if (identical(current, shown)) NULL else current
}

#' Edit text live in a Shiny app with a bslib code editor
#'
#' Spins up a single-purpose Shiny app whose only control is a
#' [bslib::input_code_editor()] populated with the text at `at`, plus a
#' **Close** button. While it runs, editor changes are streamed (debounced)
#' into the live document and pushed, and remote changes are polled back into
#' the editor. Blocks until the app exits, returning the document's final
#' content.
#'
#' @param doc An `amsync_doc` handle.
#' @param at Character path to the text object.
#' @param push A zero-argument function that pushes local changes.
#' @param ext File extension used to choose the syntax-highlighting language.
#' @param debounce Milliseconds to debounce outgoing editor changes.
#'
#' @return The document's final text content (character scalar).
#'
#' @noRd
edit_in_shiny <- function(doc, at, push, ext = NULL, debounce = 300L) {
  if (
    !requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE)
  ) {
    stop(
      "amsync_edit() requires the 'shiny' and 'bslib' packages.\n",
      'Install them with install.packages(c("shiny", "bslib")).'
    )
  }

  # How often (ms) to poll the live document for remote changes.
  poll_ms <- 250L
  base <- am_text_content(navigate_to_text(doc$doc, at))

  ui <- bslib::page_fillable(
    title = "amsync_edit",
    padding = 0,
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span("Edit synced text (live)"),
        shiny::actionButton(
          "close",
          "Close",
          class = "btn-sm btn-outline-secondary"
        )
      ),
      bslib::card_body(
        padding = 0,
        bslib::input_code_editor(
          "content",
          value = base,
          language = ext_to_language(ext),
          fill = TRUE
        )
      )
    ),
    # input_code_editor() only flushes its value to R on blur / Ctrl+Enter.
    # This shim hooks the underlying Prism editor's per-change "update" event
    # and flushes the value (via the binding's onChangeCallback) on a debounce,
    # giving real-time outgoing sync without losing syntax highlighting.
    shiny::tags$script(shiny::HTML(editor_stream_js(debounce)))
  )

  server <- function(input, output, session) {
    # `shown` is the content both the editor and document last agreed on. It
    # lets each side ignore the echo of its own write: an outgoing edit sets it
    # to what we wrote, the poll skips while the document still matches it.
    state <- new.env(parent = emptyenv())
    state$shown <- base

    # Outgoing: debounced editor changes -> minimal diff -> push.
    shiny::observeEvent(
      input$content,
      {
        target <- navigate_to_text(doc$doc, at)
        state$shown <- sync_editor_to_doc(
          target,
          input$content %||% "",
          base,
          push
        )
      },
      ignoreInit = TRUE
    )

    # Incoming: poll the live document; reflect remote changes into the editor.
    shiny::observe({
      shiny::invalidateLater(poll_ms)
      target <- navigate_to_text(doc$doc, at)
      current <- poll_doc_to_editor(target, state$shown)
      if (!is.null(current)) {
        state$shown <- current
        bslib::update_code_editor("content", value = current)
      }
    })

    # Stop exactly once; the Close button or window-close ends the session.
    done <- FALSE
    finish <- function() {
      if (done) {
        return()
      }
      done <<- TRUE
      shiny::stopApp(am_text_content(navigate_to_text(doc$doc, at)))
    }
    shiny::observeEvent(input$close, finish())
    session$onSessionEnded(function() finish())
  }

  shiny::runGadget(shiny::shinyApp(ui, server), stopOnCancel = FALSE)
}

#' JavaScript that streams the code editor's contents to Shiny on a debounce
#'
#' Waits for the `<bslib-code-editor id="content">` web component and its
#' underlying Prism editor, then forwards every content change to Shiny through
#' the input binding's `onChangeCallback`, debounced by `debounce` ms.
#'
#' @param debounce Debounce delay in milliseconds.
#'
#' @return A character scalar of JavaScript.
#'
#' @noRd
editor_stream_js <- function(debounce) {
  sprintf(
    "(function() {
  var DEBOUNCE = %d;
  function init() {
    var el = document.getElementById('content');
    if (!el || !el.prismEditor) { setTimeout(init, 50); return; }
    var timer = null;
    el.prismEditor.on('update', function() {
      if (timer) { clearTimeout(timer); }
      timer = setTimeout(function() {
        timer = null;
        el.onChangeCallback(false);
      }, DEBOUNCE);
    });
  }
  init();
})();",
    as.integer(debounce)
  )
}

#' Map a file extension to a code-editor language
#'
#' Returns one of the languages supported by [bslib::input_code_editor()],
#' falling back to `"plain"` for unknown or missing extensions.
#'
#' @param ext File extension, with or without a leading dot, or `NULL`.
#'
#' @return A character scalar language name.
#'
#' @noRd
ext_to_language <- function(ext) {
  if (is.null(ext) || !nzchar(ext)) {
    return("plain")
  }
  switch(
    tolower(sub("^\\.", "", ext)),
    r = ,
    rprofile = "r",
    py = "python",
    jl = "julia",
    sql = "sql",
    js = ,
    mjs = ,
    cjs = ,
    jsx = "javascript",
    ts = ,
    tsx = "typescript",
    htm = ,
    html = "html",
    css = "css",
    scss = "scss",
    sass = "sass",
    json = "json",
    md = ,
    markdown = ,
    qmd = ,
    rmd = "markdown",
    yml = ,
    yaml = "yaml",
    svg = ,
    xml = "xml",
    toml = "toml",
    cfg = ,
    conf = ,
    ini = "ini",
    sh = ,
    zsh = ,
    bash = "bash",
    dockerfile = "docker",
    tex = ,
    latex = "latex",
    c = ,
    h = ,
    cc = ,
    hh = ,
    cxx = ,
    hpp = ,
    cpp = "cpp",
    rs = "rust",
    patch = ,
    diff = "diff",
    "plain"
  )
}

#' Preserve the base string's trailing-newline state
#'
#' If `base` did not end in a newline, strip any trailing newline(s) the
#' editor appended; otherwise leave `edited` unchanged.
#'
#' @noRd
match_trailing_newline <- function(edited, base) {
  base_has_nl <- grepl("\n$", base)
  if (!base_has_nl) {
    edited <- sub("\n+$", "", edited)
  }
  edited
}
