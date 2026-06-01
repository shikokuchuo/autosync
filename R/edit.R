# Edit a synced Automerge text object in the user's $EDITOR

#' Edit a synced text object in your editor
#'
#' Opens a synced Automerge text object in the user's editor. On save, the
#' edits are merged back into the live document and pushed to the server,
#' preserving any concurrent remote edits that arrived while editing.
#'
#' @param client An `amsync_client` object (see [amsync_client()]) with an
#'   active connection.
#' @param at Character path to the text object within the document. A single
#'   string (e.g. `"text"`) addresses a top-level key; a character vector
#'   (e.g. `c("files", "x")`) navigates nested objects with `[[`. Default
#'   `"text"`.
#' @param editor Editor command to launch. If `NULL` (default), resolved from
#'   `getOption("editor")`, then the `VISUAL` and `EDITOR` environment
#'   variables, then a platform fallback. GUI editors must be invoked in a
#'   blocking mode (e.g. `"code --wait"`, `"subl -w"`).
#' @param ext File extension (including the leading dot, e.g. `".md"`) for the
#'   temporary file, used by editors for syntax highlighting. Default `".txt"`.
#'
#' @return Invisibly returns `client`.
#'
#' @details
#' The document is forked at its current snapshot, the text object inside the
#' fork is opened in the editor, and on save the fork is merged back into the
#' live document. Because Automerge resolves the merge, edits made remotely
#' while you were editing are preserved rather than overwritten.
#'
#' The temporary file is written and read as raw UTF-8 bytes, so the content
#' round-trips faithfully. The original's trailing-newline state is preserved:
#' if the text did not end in a newline, any trailing newline(s) the editor
#' appended are stripped; if it did, the saved content is left as-is.
#'
#' @examplesIf interactive()
#' server <- amsync_server()
#' server$start()
#' doc_id <- create_document(server)
#' doc <- get_document(server, doc_id)
#' doc$text <- automerge::am_text("edit me")
#'
#' client <- amsync_client(server$url, doc_id)
#' amsync_edit(client, at = "text", ext = ".md")
#'
#' client$close()
#' server$close()
#'
#' @importFrom automerge am_fork am_merge am_text_content am_text_update
#' @export
amsync_edit <- function(client, at = "text", editor = NULL, ext = NULL) {
  if (!inherits(client, "amsync_client")) {
    stop("`client` must be an `amsync_client` object")
  }
  if (!isTRUE(client$active)) {
    stop("`client` is not active; reconnect with amsync_client()")
  }
  if (!is.character(at) || !length(at) || anyNA(at) || any(!nzchar(at))) {
    stop("`at` must be a non-empty character path")
  }
  ext <- ext %||% ".txt"

  # Fork at the current snapshot (shares history) and resolve the text object
  # inside the fork.
  fork <- am_fork(client$doc)
  target <- navigate_to_text(fork, at)
  base <- am_text_content(target)

  tmp <- tempfile(fileext = ext)
  on.exit(unlink(tmp), add = TRUE)
  write_text_file(tmp, base)

  launch_editor(tmp, editor)

  edited <- read_text_file(tmp)
  edited <- match_trailing_newline(edited, base)

  if (identical(edited, base)) {
    message("No changes.")
    return(invisible(client))
  }

  # Apply the minimal diff to the FORK, then drain any buffered remote
  # messages before merging so concurrent edits are part of the live doc.
  am_text_update(target, edited)
  for (i in seq_len(3L)) run_now()
  am_merge(client$doc, fork)
  client$push()

  message(sprintf(
    "Updated %s: %d -> %d chars; pushed.",
    paste(at, collapse = "/"),
    nchar(base, type = "bytes"),
    nchar(edited, type = "bytes")
  ))
  invisible(client)
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

#' Resolve the editor command
#'
#' Precedence: explicit `editor` arg, `getOption("editor")`, `$VISUAL`,
#' `$EDITOR`, then a platform fallback (`notepad` on Windows, `vi` elsewhere).
#'
#' @param editor Explicit editor command, or `NULL`.
#'
#' @return A character string editor command.
#'
#' @noRd
resolve_editor <- function(editor = NULL) {
  candidates <- list(
    editor,
    getOption("editor"),
    Sys.getenv("VISUAL", unset = NA_character_),
    Sys.getenv("EDITOR", unset = NA_character_)
  )
  for (cand in candidates) {
    # `getOption("editor")` may be a function (e.g. in RStudio); only a
    # single, non-empty string is usable as a shell command here.
    if (is.character(cand) && length(cand) == 1L && !is.na(cand) && nzchar(cand)) {
      return(cand)
    }
  }
  if (.Platform$OS.type == "windows") "notepad" else "vi"
}

#' Launch a blocking editor on a file
#'
#' @param path Path to the file to edit.
#' @param editor Editor command, or `NULL` to resolve automatically.
#'
#' @return Invisibly `NULL`. Errors if the editor exits non-zero.
#'
#' @noRd
launch_editor <- function(path, editor = NULL) {
  cmd <- resolve_editor(editor)
  # Split the command so flags like "code --wait" are passed as arguments.
  parts <- strsplit(cmd, "\\s+")[[1]]
  status <- system2(
    parts[1],
    args = c(parts[-1], shQuote(path)),
    wait = TRUE
  )
  if (!identical(status, 0L)) {
    stop("Editor (", cmd, ") exited with status ", status)
  }
  invisible()
}

#' Write a string to a file as raw UTF-8 bytes, verbatim
#'
#' @noRd
write_text_file <- function(path, text) {
  writeBin(charToRaw(enc2utf8(text)), path)
}

#' Read a file as a UTF-8 string, verbatim
#'
#' @noRd
read_text_file <- function(path) {
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  text
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
