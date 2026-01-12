#' autosync: Automerge Sync Server for R
#'
#' A WebSocket-based synchronization server for Automerge documents,
#' compatible with the automerge-repo protocol used by sync.automerge.org.
#' Enables R to serve as a sync hub for JavaScript, Rust, and other Automerge
#' clients in collaborative applications.
#'
#' @section Main Functions:
#' \describe{
#'   \item{[amsync_server()]}{Create a new sync server with `$start()` and
#'     `$stop()` methods}
#' }
#'
#' @section Document Management:
#' \describe{
#'   \item{[create_document()]}{Create a new document}
#'   \item{[get_document()]}{Retrieve a document by ID}
#'   \item{[list_documents()]}{List all document IDs}
#'   \item{[generate_document_id()]}{Generate a new document ID}
#' }
#'
#' @section Protocol:
#' The server implements the automerge-repo sync protocol over WebSockets.
#' Messages are CBOR-encoded and include:
#' \describe{
#'   \item{join/peer}{Handshake messages for connection establishment}
#'   \item{request/sync}{Document synchronization messages}
#'   \item{ephemeral}{Transient messages forwarded without persistence}
#'   \item{error}{Error notifications}
#' }
#'
#' @section Example:
#' \preformatted{
#' # Create and start a server
#' server <- amsync_server(port = 3030)
#' server$start()
#'
#' # Stop when done
#' server$stop()
#' }
#'
#' @docType package
#' @name autosync-package
#' @aliases autosync
#'
#' @importFrom automerge am_create am_get am_keys am_length am_load am_save am_sync_decode am_sync_encode am_sync_state_new
#' @importFrom nanonext http_server random recv send stream tls_config write_cert
#' @importFrom secretbase base64enc base64dec base58enc base58dec cborenc cbordec
"_PACKAGE"
