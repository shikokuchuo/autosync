# Document storage functions for autosync

#' Save a document to disk
#'
#' @param server An amsync_server object.
#' @param doc_id Document ID string.
#' @param doc Automerge document object.
#'
#' @noRd
save_document <- function(server, doc_id, doc) {
  path <- file.path(server$data_dir, paste0(doc_id, ".automerge"))
  writeBin(am_save(doc), path)
}

#' Load a document from disk
#'
#' @param server An amsync_server object.
#' @param doc_id Document ID string.
#'
#' @return Automerge document object, or NULL if not found.
#'
#' @noRd
load_document <- function(server, doc_id) {
  path <- file.path(server$data_dir, paste0(doc_id, ".automerge"))
  if (file.exists(path)) {
    am_load(readBin(path, "raw", file.info(path)$size))
  } else {
    NULL
  }
}

#' Load all documents from disk
#'
#' Loads all .automerge files from the server's data directory
#' into the documents environment.
#'
#' @param server An amsync_server object.
#'
#' @noRd
load_all_documents <- function(server) {
  files <- list.files(server$data_dir, pattern = "\\.automerge$", full.names = TRUE)
  for (f in files) {
    doc_id <- sub("\\.automerge$", "", basename(f))
    tryCatch({
      server$documents[[doc_id]] <- am_load(
        readBin(f, "raw", file.info(f)$size)
      )
    }, error = function(e) {
      warning("Failed to load document ", doc_id, ": ", conditionMessage(e))
    })
  }
}
