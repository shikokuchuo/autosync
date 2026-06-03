# Project browsing: file-tree navigation from a project document ID

#' Browse and edit files in a project document
#'
#' Given a sync server URL and a project document ID, opens a persistent
#' connection to the server, syncs the project document over it, and exposes
#' its file tree for browsing and editing. A project is an Automerge document
#' with a `files` map whose keys are file paths and whose values are text
#' objects holding each file's own document ID.
#'
#' Opening or editing a file syncs that file's document over the **same**
#' connection rather than dialing the server again, so a browse session reuses
#' a single WebSocket throughout. Call `$close()` when finished to disconnect.
#'
#' @inheritParams amsync_fetch
#' @param proj_id Document ID of the project.
#' @param files_key Key of the files map within the project document. Default
#'   `"files"`.
#'
#' @return An environment of class `"amsync_project"` (reference semantics)
#'   with the following fields and methods:
#'   \describe{
#'     \item{`doc`}{The live project document, kept in sync with the server.}
#'     \item{`conn`}{The underlying [amsync_client()] connection.}
#'     \item{`paths()`}{Current sorted file paths.}
#'     \item{`doc_id(path)`}{Resolve a path to its document ID.}
#'     \item{`open(path)`}{Open the file's document over the project connection
#'       and return its `amsync_doc` handle. Reuses the connection and any
#'       already-open document.}
#'     \item{`edit(path = NULL)`}{Open the file's document and run
#'       [amsync_edit()] with the extension inferred from the path. If `path`
#'       is `NULL` and interactive, shows a Shiny file picker first.}
#'     \item{`browse()`}{Interactive loop: pick a file from a Shiny file
#'       picker, edit it, then return to the picker; repeat until **Done**.}
#'     \item{`refresh()`}{Re-resolve the file tree to pick up added or removed
#'       files (the project document syncs live, so this just settles pending
#'       updates).}
#'     \item{`close()`}{Disconnect the project connection.}
#'   }
#'
#' @examplesIf interactive()
#' proj <- amsync_project("wss://quarto-hub.com/ws", proj_id, token = amsync_token())
#' proj                                   # prints the file tree
#' proj$browse()                          # pick a file, edit it, repeat
#' proj$edit("/charlie/index.qmd")        # edit a known path directly
#' proj$close()                           # disconnect when finished
#'
#' @importFrom automerge am_keys am_text_content
#' @importFrom tools file_ext
#' @export
amsync_project <- function(
  url,
  proj_id,
  token = NULL,
  tls = NULL,
  timeout = 5000L,
  files_key = "files"
) {
  conn <- amsync_client(url, timeout = timeout, tls = tls, token = token)

  # Sync the project document over the connection, then validate its files map.
  # Tear the connection down if either step fails so we never leak a socket.
  proj_doc <- tryCatch(
    conn$open_doc(proj_id),
    error = function(e) {
      conn$close()
      stop(e)
    }
  )
  files_map <- tryCatch(
    resolve_files_map(proj_doc$doc, files_key),
    error = function(e) {
      conn$close()
      stop(e)
    }
  )

  proj <- new.env(parent = emptyenv())
  proj$conn <- conn
  proj$doc <- proj_doc$doc
  proj$url <- url
  proj$proj_id <- proj_id
  proj$files_key <- files_key
  proj$files_map <- files_map

  proj$paths <- function() {
    sort(am_keys(proj$doc, proj$files_map))
  }

  proj$doc_id <- function(path) {
    entry <- proj$files_map[[path]]
    if (is.null(entry)) {
      stop(
        "Unknown path: ", path, "\nAvailable paths:\n  ",
        paste(proj$paths(), collapse = "\n  ")
      )
    }
    if (!inherits(entry, "am_text")) {
      stop("Entry at ", path, " is not a text object")
    }
    am_text_content(entry)
  }

  proj$open <- function(path) {
    proj$conn$open_doc(proj$doc_id(path))
  }

  proj$edit <- function(path = NULL) {
    if (is.null(path)) {
      if (!interactive()) {
        stop("`path` is required in non-interactive sessions")
      }
      path <- pick_path_shiny(proj$paths())
      if (is.null(path)) {
        return(invisible(proj))
      }
    }
    ext <- file_ext_dot(path)
    amsync_edit(proj$open(path), at = "text", ext = ext)
    invisible(proj)
  }

  proj$browse <- function() {
    if (!interactive()) {
      stop("`$browse()` requires an interactive session; pass a path to `$edit()`")
    }
    repeat {
      path <- pick_path_shiny(proj$paths())
      if (is.null(path)) {
        break
      }
      proj$edit(path)
    }
    invisible(proj)
  }

  proj$refresh <- function() {
    # The project document syncs live over the connection; settle any pending
    # updates, then re-resolve the files map to reflect added/removed files.
    for (i in seq_len(5L)) {
      run_now(0.05)
    }
    proj$files_map <- resolve_files_map(proj$doc, proj$files_key)
    invisible(proj)
  }

  proj$close <- function() {
    proj$conn$close()
    invisible(proj)
  }

  class(proj) <- "amsync_project"
  proj
}

#' @export
print.amsync_project <- function(x, ...) {
  paths <- x$paths()
  cat("Automerge Project\n")
  cat("  Project:", x$proj_id, "\n")
  cat("  Server:", x$url, "\n")
  cat("  Files:", length(paths), "\n\n")
  cat(format_file_tree(paths))
  cat("\nCall $browse() to pick a file and edit it, $close() when done.\n")
  invisible(x)
}

#' Resolve and validate the files map within a project document
#'
#' @param doc The project document.
#' @param files_key Key of the files map.
#'
#' @return The `am_map` object.
#'
#' @noRd
resolve_files_map <- function(doc, files_key) {
  files_map <- doc[[files_key]]
  if (is.null(files_map)) {
    stop("Project document has no `", files_key, "` map")
  }
  if (!inherits(files_map, "am_map")) {
    stop("`", files_key, "` is not a map")
  }
  files_map
}

#' File extension as a dotted suffix for tempfile()
#'
#' @param path A file path.
#'
#' @return `".ext"`, or `".txt"` if the path has no extension.
#'
#' @noRd
file_ext_dot <- function(path) {
  ext <- tools::file_ext(path)
  if (nzchar(ext)) paste0(".", ext) else ".txt"
}

#' Pick a file path from a project's tree in a Shiny app
#'
#' Spins up a single-purpose Shiny app presenting the project's file paths as a
#' radio-button list, with **Edit** and **Done** buttons. Blocks until the app
#' exits, returning the selected path on **Edit** or `NULL` if the user chose
#' **Done** or closed the window.
#'
#' @param paths Character vector of paths.
#'
#' @return The selected path (character scalar), or `NULL` if not selected.
#'
#' @noRd
pick_path_shiny <- function(paths) {
  if (!length(paths)) {
    message("No files in project.")
    return(NULL)
  }
  if (
    !requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE)
  ) {
    stop(
      "Picking a file interactively requires the 'shiny' and 'bslib' packages.\n",
      'Install them with install.packages(c("shiny", "bslib")).'
    )
  }

  ui <- bslib::page_fillable(
    title = "amsync_project",
    padding = 0,
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        shiny::span("Select a file to edit"),
        shiny::div(
          class = "d-flex gap-2",
          shiny::actionButton(
            "done",
            "Done",
            class = "btn-sm btn-outline-secondary"
          ),
          shiny::actionButton("edit", "Edit", class = "btn-sm btn-primary")
        )
      ),
      bslib::card_body(
        shiny::radioButtons("path", label = NULL, choices = paths)
      )
    )
  )

  server <- function(input, output, session) {
    # Stop exactly once; whichever of Edit / Done / window-close happens first
    # decides the return value.
    done <- FALSE
    finish <- function(value) {
      if (done) {
        return()
      }
      done <<- TRUE
      shiny::stopApp(returnValue = value)
    }
    shiny::observeEvent(input$edit, finish(input$path))
    shiny::observeEvent(input$done, finish(NULL))
    session$onSessionEnded(function() finish(NULL))
  }

  shiny::runGadget(shiny::shinyApp(ui, server), stopOnCancel = FALSE)
}

#' Render a set of file paths as an indented tree
#'
#' @param paths Character vector of file paths.
#'
#' @return A single string with a trailing newline.
#'
#' @noRd
format_file_tree <- function(paths) {
  if (!length(paths)) {
    return("/\n")
  }
  tree <- list()
  for (p in paths) {
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    tree <- tree_insert(tree, parts)
  }
  lines <- c("/", tree_render(tree, prefix = ""))
  paste0(paste(lines, collapse = "\n"), "\n")
}

#' Insert a path's components into the nested tree
#'
#' @noRd
tree_insert <- function(tree, parts) {
  if (!length(parts)) {
    return(tree)
  }
  head <- parts[[1]]
  if (is.null(tree[[head]])) {
    tree[[head]] <- list()
  }
  tree[[head]] <- tree_insert(tree[[head]], parts[-1])
  tree
}

#' Render one level of the nested tree
#'
#' Directories (nodes with children) sort before files, alphabetical within
#' each group.
#'
#' @noRd
tree_render <- function(node, prefix) {
  keys <- names(node)
  is_dir <- vapply(keys, function(k) length(node[[k]]) > 0L, logical(1))
  ord <- order(!is_dir, keys)
  keys <- keys[ord]
  is_dir <- is_dir[ord]

  # Box-drawing glyphs as \u escapes to keep the source ASCII-only:
  # tee = branch, elbow = last child, pipe = vertical continuation.
  tee <- "\u251c\u2500 "
  elbow <- "\u2514\u2500 "
  pipe <- "\u2502  "

  lines <- character(0)
  n <- length(keys)
  for (i in seq_len(n)) {
    last <- i == n
    connector <- if (last) elbow else tee
    label <- if (is_dir[i]) paste0(keys[i], "/") else keys[i]
    lines <- c(lines, paste0(prefix, connector, label))
    if (is_dir[i]) {
      child_prefix <- paste0(prefix, if (last) "   " else pipe)
      lines <- c(lines, tree_render(node[[keys[i]]], child_prefix))
    }
  }
  lines
}
