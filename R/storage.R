# Document and sync state storage functions for autosync

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

#' Save sync state to disk
#'
#' Persists sync state for a peer with a storageId, enabling incremental
#' sync on reconnection.
#'
#' @param server Server state environment.
#' @param storage_id The peer's storage ID.
#' @param doc_id Document ID.
#' @param sync_state Automerge sync state object.
#'
#' @noRd
save_sync_state <- function(server, storage_id, doc_id, sync_state) {
  dir <- file.path(server$data_dir, ".sync_states", storage_id)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  path <- file.path(dir, paste0(doc_id, ".sync"))
  tryCatch(
    writeBin(am_sync_state_encode(sync_state), path),
    error = function(e) {
      warning("Failed to save sync state: ", conditionMessage(e))
    }
  )
}

#' Load persisted sync states for a peer
#'
#' Loads all persisted sync states for a peer identified by storageId.
#'
#' @param server Server state environment.
#' @param storage_id The peer's storage ID.
#' @param client_id The peer's current client ID.
#'
#' @noRd
load_sync_states <- function(server, storage_id, client_id) {
  dir <- file.path(server$data_dir, ".sync_states", storage_id)
  if (!dir.exists(dir)) {
    return(invisible())
  }
  files <- list.files(dir, pattern = "\\.sync$", full.names = TRUE)
  if (!length(files)) {
    return(invisible())
  }

  client_states <- server$sync_states[[client_id]]
  if (is.null(client_states)) {
    client_states <- new.env(hash = TRUE, parent = emptyenv())
    server$sync_states[[client_id]] <- client_states
  }

  for (f in files) {
    doc_id <- sub("\\.sync$", "", basename(f))
    tryCatch({
      sync_state <- am_sync_state_decode(
        readBin(f, "raw", file.info(f)$size)
      )
      client_states[[doc_id]] <- sync_state
    }, error = function(e) {
      warning("Failed to load sync state for ", doc_id, ": ", conditionMessage(e))
    })
  }

  invisible()
}
