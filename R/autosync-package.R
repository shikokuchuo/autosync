#' autosync: 'Automerge' Sync Server and Client for R
#'
#' A WebSocket-based implementation of the 'automerge-repo' synchronization
#' protocol used by 'sync.automerge.org'. Acts as a sync server, enabling R to
#' serve as a synchronization hub for 'Automerge' clients in 'JavaScript',
#' 'Rust', and other languages, and as a client for fetching, editing, and
#' synchronizing documents hosted on remote servers.
#'
#' @section Main Functions:
#' \describe{
#'   \item{[sync_server()]}{Create a new sync server with `$start()` and
#'     `$close()` methods}
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
#' @section Browsing and Editing:
#' \describe{
#'   \item{[amsync_app()]}{Launch a single-window Shiny app to connect to a
#'     project, browse its file tree, and edit files in a live code editor}
#'   \item{[amsync_project()]}{Open the files in a project document}
#'   \item{`$edit()`}{The document handle from [sync_client()]'s `$open_doc()`
#'     edits a synced text object in a live Shiny code editor}
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
#' server <- sync_server()
#' server$start()
#' server$url
#'
#' # Stop when done
#' server$close()
#' }
#'
#' @docType package
#' @name autosync-package
#' @aliases autosync
#'
#' @importFrom automerge am_create am_get am_keys am_length am_load am_save am_sync_decode am_sync_encode am_sync_state am_sync_state_decode am_sync_state_encode AM_ROOT
#' @importFrom httr2 oauth_client oauth_flow_auth_code oauth_redirect_uri oauth_server_metadata
#' @importFrom later later run_now
#' @importFrom nanonext http_server random recv recv_aio send stop_aio stream tls_config unresolved write_cert
#' @importFrom secretbase base64enc base64dec base58enc base58dec cborenc cbordec jsondec jsonenc
#' @importFrom utils str
"_PACKAGE"
