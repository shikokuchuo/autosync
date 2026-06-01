# Project browsing: file-tree navigation from a project document ID

#' Browse and edit files in a project document
#'
#' Given a sync server URL and a project document ID, fetches the project and
#' exposes its file tree for browsing and editing. A project is an Automerge
#' document with a `files` map whose keys are file paths and whose values are
#' text objects holding each file's own document ID.
#'
#' @inheritParams amsync_fetch
#' @param proj_id Document ID of the project.
#' @param files_key Key of the files map within the project document. Default
#'   `"files"`.
#'
#' @return An environment of class `"amsync_project"` (reference semantics)
#'   with the following fields and methods:
#'   \describe{
#'     \item{`doc`}{The fetched project document.}
#'     \item{`paths()`}{Current sorted file paths.}
#'     \item{`doc_id(path)`}{Resolve a path to its document ID.}
#'     \item{`open(path)`}{Return a live [amsync_client()] for the file's
#'       document (the caller owns it and must `$close()` it).}
#'     \item{`edit(path = NULL)`}{Open the file's client, run [amsync_edit()]
#'       with the extension inferred from the path, then close the client. If
#'       `path` is `NULL` and interactive, shows the picker first.}
#'     \item{`browse()`}{Interactive loop: print the tree, pick a file, edit
#'       it, repeat.}
#'     \item{`refresh()`}{Re-fetch the project document to pick up added or
#'       removed files.}
#'   }
#'
#' @examplesIf interactive()
#' proj <- amsync_project("wss://quarto-hub.com/ws", proj_id, token = amsync_token())
#' proj                                   # prints the file tree
#' proj$browse()                          # pick a file, edit it, repeat
#' proj$edit("/charlie/index.qmd")        # edit a known path directly
#'
#' @importFrom automerge am_keys am_text_content
#' @importFrom tools file_ext
#' @importFrom utils menu
#' @export
amsync_project <- function(
  url,
  proj_id,
  token = NULL,
  tls = NULL,
  timeout = 5000L,
  files_key = "files"
) {
  doc <- amsync_fetch(url, proj_id, timeout = timeout, tls = tls, token = token)

  proj <- new.env(parent = emptyenv())
  proj$doc <- doc
  proj$url <- url
  proj$proj_id <- proj_id
  proj$token <- token
  proj$tls <- tls
  proj$timeout <- timeout
  proj$files_key <- files_key
  proj$files_map <- resolve_files_map(doc, files_key)

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
    amsync_client(
      proj$url,
      proj$doc_id(path),
      timeout = proj$timeout,
      tls = proj$tls,
      token = proj$token
    )
  }

  proj$edit <- function(path = NULL) {
    if (is.null(path)) {
      if (!interactive()) {
        stop("`path` is required in non-interactive sessions")
      }
      path <- pick_path(proj$paths())
      if (is.null(path)) {
        return(invisible(proj))
      }
    }
    ext <- file_ext_dot(path)
    client <- proj$open(path)
    on.exit(client$close())
    amsync_edit(client, at = "text", ext = ext)
    invisible(proj)
  }

  proj$browse <- function() {
    if (!interactive()) {
      stop("`$browse()` requires an interactive session; pass a path to `$edit()`")
    }
    repeat {
      cat(format_file_tree(proj$paths()))
      path <- pick_path(proj$paths())
      if (is.null(path)) {
        break
      }
      proj$edit(path)
    }
    invisible(proj)
  }

  proj$refresh <- function() {
    proj$doc <- amsync_fetch(
      proj$url,
      proj$proj_id,
      timeout = proj$timeout,
      tls = proj$tls,
      token = proj$token
    )
    proj$files_map <- resolve_files_map(proj$doc, proj$files_key)
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
  cat("\nCall $browse() to pick a file and edit it.\n")
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

#' Interactively pick a path from a menu
#'
#' @param paths Character vector of paths.
#'
#' @return The selected path, or `NULL` if the user quit (0).
#'
#' @noRd
pick_path <- function(paths) {
  if (!length(paths)) {
    message("No files in project.")
    return(NULL)
  }
  choice <- utils::menu(paths, title = "Select a file (0 to quit):")
  if (choice == 0L) NULL else paths[choice]
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
