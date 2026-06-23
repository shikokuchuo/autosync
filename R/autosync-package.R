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
#' @importFrom jose jwt_decode_sig read_jwk
#' @importFrom later later run_now
#' @importFrom nanonext handler_ws http_server is_error_value ncurl random recv recv_aio send stop_aio stream tls_config unresolved write_cert
#' @importFrom promises then
#' @importFrom secretbase base64enc base64dec base58enc base58dec cborenc cbordec jsondec jsonenc
#' @importFrom utils str
"_PACKAGE"
