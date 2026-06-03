# Edit a synced Automerge text object in a Shiny code editor

#' Edit a synced text object in a Shiny editor
#'
#' Opens a synced Automerge text object in a Shiny app featuring a
#' [bslib::input_code_editor()] component. Clicking **Save** closes the app;
#' the edited text is then merged back into the live document and pushed to the
#' server, preserving any concurrent remote edits that arrived while editing.
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
#'
#' @return Invisibly returns `doc`.
#'
#' @details
#' Requires the \pkg{shiny} and \pkg{bslib} packages.
#'
#' The document is forked at its current snapshot, the text object inside the
#' fork is opened in the editor, and on **Save** the fork is merged back into
#' the live document. Because Automerge resolves the merge, edits made remotely
#' while you were editing are preserved rather than overwritten. Closing the
#' app without saving (the **Cancel** button or closing the window) leaves the
#' document untouched.
#'
#' The original's trailing-newline state is preserved: if the text did not end
#' in a newline, any trailing newline(s) the editor appended are stripped; if
#' it did, the saved content is left as-is.
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
#' @importFrom automerge am_fork am_merge am_text_content am_text_update
#' @export
amsync_edit <- function(doc, at = "text", ext = NULL) {
  if (!inherits(doc, "amsync_doc")) {
    stop("`doc` must be an `amsync_doc` object (see `amsync_client()$open_doc()`)")
  }
  if (!isTRUE(doc$active)) {
    stop("`doc` is not active; reopen it with `$open_doc()`")
  }
  if (!is.character(at) || !length(at) || anyNA(at) || any(!nzchar(at))) {
    stop("`at` must be a non-empty character path")
  }

  # Fork at the current snapshot (shares history) and resolve the text object
  # inside the fork.
  fork <- am_fork(doc$doc)
  target <- navigate_to_text(fork, at)
  base <- am_text_content(target)

  # Edit in the Shiny app. While it runs the connection's async loops keep
  # firing, so concurrent remote edits land on the live doc; the merge below
  # folds them in. `NULL` means the user closed the app without saving.
  edited <- edit_in_shiny(base, ext)
  if (is.null(edited)) {
    message("Edit cancelled.")
    return(invisible(doc))
  }
  edited <- match_trailing_newline(enc2utf8(edited), base)

  if (identical(edited, base)) {
    message("No changes.")
    return(invisible(doc))
  }

  # Apply the minimal diff to the FORK, then drain any buffered remote
  # messages before merging so concurrent edits are part of the live doc.
  am_text_update(target, edited)
  for (i in seq_len(3L)) run_now()
  am_merge(doc$doc, fork)
  doc$push()

  message(sprintf(
    "Updated %s: %d -> %d chars; pushed.",
    paste(at, collapse = "/"),
    nchar(base, type = "bytes"),
    nchar(edited, type = "bytes")
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

#' Edit text in a Shiny app with a bslib code editor
#'
#' Spins up a single-purpose Shiny app whose only control is a
#' [bslib::input_code_editor()] populated with `text`, plus **Save** and
#' **Cancel** buttons. Blocks until the app exits, returning the editor's
#' contents on save or `NULL` if the user cancelled or closed the window.
#'
#' @param text The initial text to edit.
#' @param ext File extension used to choose the syntax-highlighting language.
#'
#' @return The edited text (character scalar), or `NULL` if not saved.
#'
#' @noRd
edit_in_shiny <- function(text, ext = NULL) {
  if (
    !requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE)
  ) {
    stop(
      "amsync_edit() requires the 'shiny' and 'bslib' packages.\n",
      'Install them with install.packages(c("shiny", "bslib")).'
    )
  }

  ui <- bslib::page_fillable(
    title = "amsync_edit",
    padding = 0,
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span("Edit synced text"),
        shiny::div(
          class = "d-flex gap-2",
          shiny::actionButton(
            "cancel",
            "Cancel",
            class = "btn-sm btn-outline-secondary"
          ),
          shiny::actionButton("save", "Save", class = "btn-sm btn-primary")
        )
      ),
      bslib::card_body(
        padding = 0,
        bslib::input_code_editor(
          "content",
          value = text,
          language = ext_to_language(ext),
          fill = TRUE
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Stop exactly once; whichever of Save / Cancel / window-close happens
    # first decides the return value. The editor flushes its value to the
    # server on blur, so clicking Save makes `input$content` current.
    done <- FALSE
    finish <- function(value) {
      if (done) {
        return()
      }
      done <<- TRUE
      shiny::stopApp(returnValue = value)
    }
    shiny::observeEvent(input$save, finish(input$content %||% text))
    shiny::observeEvent(input$cancel, finish(NULL))
    session$onSessionEnded(function() finish(NULL))
  }

  shiny::runGadget(shiny::shinyApp(ui, server), stopOnCancel = FALSE)
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
