# Edit a synced Automerge text object in a live Shiny code editor

#' Run the live editor for an open document handle
#'
#' Implements the `$edit()` method on the `amsync_doc` handle returned by
#' [amsync_client()]'s `$open_doc()`: opens the document's text object at `at`
#' in a Shiny [bslib::input_code_editor()] that stays in sync with the live
#' document in both directions, blocks until the editor closes, then prints a
#' one-line summary. The user-facing description and caveats live on
#' [amsync_client()].
#'
#' @param doc An `amsync_doc` handle backed by an active connection.
#' @param at Character path to the text object within the document. A single
#'   string addresses a top-level key; a character vector navigates nested
#'   objects with `[[`. Default `"text"`.
#' @param ext File extension used to pick the editor's syntax-highlighting
#'   language, or `NULL` for plain text.
#' @param debounce Milliseconds to wait after the last keystroke before pushing.
#'
#' @return Invisibly returns `doc`.
#'
#' @importFrom automerge am_text_content am_text_update
#' @noRd
edit_doc <- function(doc, at = "text", ext = NULL, debounce = 300L) {
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

  final <- edit_in_shiny(doc, at, ext = ext, debounce = debounce)

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

#' Wire the bidirectional editor <-> live-document sync onto a Shiny session
#'
#' Installs the two observers shared by [edit_in_shiny()] and [amsync_app()]'s
#' browse screen: an outgoing one that writes debounced editor changes into the
#' live document and pushes them, and an incoming one that polls the document
#' and reflects remote changes back into the editor. Both read the open
#' document and its tracking state from `st` -- a plain environment, not a
#' reactive one, so editing never re-fires the observers through a reactive
#' dependency on the document.
#'
#' `st$shown` is the content the editor and document last agreed on; it lets
#' each side ignore the echo of its own write: an outgoing edit sets it to what
#' we wrote, and the poll skips while the document still matches it.
#'
#' @param input The Shiny session's `input`.
#' @param st An environment exposing `$doc` (an `amsync_doc` handle or `NULL`
#'   when nothing is open), `$at` (the text object's path), `$base` (the open
#'   content, for trailing-newline state), and a mutable `$shown`.
#' @param poll_ms How often (ms) to poll the live document for remote changes.
#'
#' @return Invisibly `NULL`.
#'
#' @importFrom automerge am_text_content am_text_update
#' @noRd
install_editor_sync <- function(input, st, poll_ms = 250L) {
  # Outgoing: debounced editor changes -> minimal diff -> push.
  shiny::observeEvent(
    input$content,
    {
      if (is.null(st$doc) || !isTRUE(st$doc$active)) {
        return()
      }
      target <- navigate_to_text(st$doc$doc, st$at)
      st$shown <- sync_editor_to_doc(
        target,
        input$content %||% "",
        st$base,
        st$doc$push
      )
    },
    ignoreInit = TRUE
  )

  # Incoming: poll the live document; reflect remote changes into the editor.
  shiny::observe({
    shiny::invalidateLater(poll_ms)
    if (is.null(st$doc) || !isTRUE(st$doc$active)) {
      return()
    }
    target <- navigate_to_text(st$doc$doc, st$at)
    current <- poll_doc_to_editor(target, st$shown)
    if (!is.null(current)) {
      st$shown <- current
      bslib::update_code_editor("content", value = current)
    }
  })

  invisible()
}

#' Build the live code editor body shared by both editors
#'
#' The [bslib::input_code_editor()] (id `"content"`) plus the streaming shim
#' that flushes its value to R on a debounce, used by both [edit_in_shiny()]
#' and [amsync_app()]'s editor card. Returned as a tag list to drop inside a
#' [bslib::card()].
#'
#' @param value The editor's initial content.
#' @param ext File extension (dotted) for syntax highlighting, or `NULL`.
#' @param debounce Milliseconds to debounce outgoing editor changes.
#'
#' @return A shiny tag list.
#'
#' @noRd
editor_body_ui <- function(value, ext, debounce) {
  shiny::tagList(
    bslib::card_body(
      padding = 0,
      bslib::input_code_editor(
        "content",
        value = value,
        language = ext_to_language(ext),
        fill = TRUE
      )
    ),
    # input_code_editor() only flushes its value to R on blur / Ctrl+Enter.
    # This shim hooks the underlying Prism editor's per-change "update" event
    # and flushes the value (via the binding's onChangeCallback) on a debounce,
    # giving real-time outgoing sync without losing syntax highlighting.
    shiny::tags$script(shiny::HTML(editor_stream_js(debounce)))
  )
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
#' @param ext File extension used to choose the syntax-highlighting language.
#' @param debounce Milliseconds to debounce outgoing editor changes.
#'
#' @return The document's final text content (character scalar).
#'
#' @noRd
edit_in_shiny <- function(doc, at, ext = NULL, debounce = 300L) {
  if (
    !requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE)
  ) {
    stop(
      "Editing a document requires the 'shiny' and 'bslib' packages.\n",
      'Install them with install.packages(c("shiny", "bslib")).'
    )
  }

  base <- am_text_content(navigate_to_text(doc$doc, at))

  ui <- bslib::page_fillable(
    title = "amsync",
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
      editor_body_ui(base, ext, debounce)
    )
  )

  server <- function(input, output, session) {
    # Editor state lives in a plain environment read by the sync observers
    # without a reactive dependency on the document; install_editor_sync() wires
    # the bidirectional editor <-> document sync onto it (and explains `shown`).
    st <- new.env(parent = emptyenv())
    st$doc <- doc
    st$at <- at
    st$base <- base
    st$shown <- base
    install_editor_sync(input, st)

    # Stop exactly once; the Close button or window-close ends the session.
    # Closing the editor returns to the file picker (in a browse loop), so it
    # is already obvious the editor has ended -- no closing message needed here.
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
