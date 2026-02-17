# Automerge sync server implementation using nanonext

#' Create an Automerge sync server
#'
#' Creates a WebSocket server that implements the automerge-repo sync protocol,
#' compatible with JavaScript, Rust, and other Automerge clients.
#'
#' @param port Port to listen on. Default 0 (binds to a random available port).
#'   The actual URL is retrieved via `server$url`.
#' @param host Host address to bind to. Default "127.0.0.1" (localhost).
#' @param data_dir Directory for persistent document storage. Default ".automerge".
#' @param auto_create_docs Logical, whether to auto-create documents when
#'   clients request unknown document IDs. Default TRUE.
#' @param storage_id Optional storage ID for this server. If NULL (default),
#'   generates a new persistent identity. Set to NA for an ephemeral server
#'   (no persistence identity).
#' @param tls (optional) for secure wss:// connections, a TLS configuration
#'   object created by [nanonext::tls_config()].
#' @param auth Optional authentication configuration created by [auth_config()].
#'   When provided, clients must include a valid OAuth2 access token
#'   in their join message's peerMetadata. Requires the gargle package.
#'   Note: TLS is required when authentication is enabled to protect tokens.
#'
#' @return An amsync_server object inheriting from 'nanoServer', with
#'   `$start()` and `$close()` methods.
#'
#' @details
#' The returned server inherits from nanonext's nanoServer class and provides
#' `$start()` and `$close()` methods for non-blocking operation.
#'
#' @examplesIf interactive()
#' # Create and start a server
#' server <- amsync_server()
#' server$start()
#'
#' # Server is now running in the background
#' # ...do other work...
#'
#' # Stop when done
#' server$close()
#'
#' # With TLS for secure connections
#' cert <- nanonext::write_cert()
#' tls <- nanonext::tls_config(server = cert$server)
#' server <- amsync_server(tls = tls)
#' server$start()
#' server$url
#' server$close()
#'
#' # Server with Google OAuth authentication (requires TLS)
#' cert <- nanonext::write_cert()
#' tls <- nanonext::tls_config(server = cert$server)
#' server <- amsync_server(
#'   tls = tls,
#'   auth = auth_config(allowed_domains = c("mycompany.com"))
#' )
#'
#' @export
amsync_server <- function(
  port = 0L,
  host = "127.0.0.1",
  data_dir = ".automerge",
  auto_create_docs = TRUE,
  storage_id = NULL,
  tls = NULL,
  auth = NULL
) {
  port <- as.integer(port)

  # Enforce TLS when authentication is enabled
  if (!is.null(auth) && is.null(tls)) {
    stop(
      "Authentication requires TLS. Provide a 'tls' configuration.\n",
      "Transmitting OAuth tokens over unencrypted connections is a security risk."
    )
  }

  scheme <- if (is.null(tls)) "ws" else "wss"
  url <- sprintf("%s://%s:%d", scheme, host, port)

  documents <- new.env(hash = TRUE, parent = emptyenv())
  sync_states <- new.env(hash = TRUE, parent = emptyenv())
  connections <- new.env(hash = TRUE, parent = emptyenv())
  doc_peers <- new.env(hash = TRUE, parent = emptyenv())

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  # Create state environment for handlers to access via closure

  state <- new.env(hash = TRUE, parent = emptyenv())
  state$host <- host
  state$data_dir <- data_dir
  state$auto_create_docs <- auto_create_docs
  state$peer_id <- generate_peer_id()
  state$storage_id <- if (is.null(storage_id)) {
    generate_peer_id()
  } else if (is.na(storage_id)) {
    NULL
  } else {
    storage_id
  }
  state$documents <- documents
  state$sync_states <- sync_states
  state$connections <- connections
  state$doc_peers <- doc_peers
  state$auth <- auth
  state$pending_auth <- new.env(hash = TRUE, parent = emptyenv())

  load_all_documents(state)

  on_open <- function(ws) {
    ws_id <- as.character(ws$id)
    state$connections[[ws_id]] <- list(
      ws = ws,
      client_id = NULL,
      metadata = NULL,
      connected_at = Sys.time()
    )

    # If auth is enabled, track connection time and schedule timeout
    if (!is.null(state$auth)) {
      state$pending_auth[[ws_id]] <- list(
        connected_at = Sys.time(),
        timeout_scheduled = TRUE
      )

      # Schedule auth timeout check using later
      later::later(
        function() {
          check_auth_timeout(state, ws_id)
        },
        delay = state$auth$auth_timeout
      )
    }
  }

  on_message <- function(ws, data) {
    ws_id <- as.character(ws$id)
    conn <- state$connections[[ws_id]]
    client_id <- if (!is.null(conn$client_id)) conn$client_id else ws_id
    handle_message(state, client_id, ws_id, data)
  }

  on_close <- function(ws) {
    ws_id <- as.character(ws$id)
    conn <- state$connections[[ws_id]]
    if (!is.null(conn)) {
      client_id <- conn$client_id
      handle_disconnect(state, client_id)
      rm(list = ws_id, envir = state$connections)
      if (!is.null(client_id) && exists(client_id, envir = state$connections)) {
        rm(list = client_id, envir = state$connections)
      }
    }
  }

  ws_handler <- nanonext::handler_ws(
    path = "/",
    on_message = on_message,
    on_open = on_open,
    on_close = on_close,
    textframes = FALSE
  )

  server <- nanonext::http_server(
    url = url,
    handlers = list(ws_handler),
    tls = tls
  )

  attr(server, "sync") <- state
  class(server) <- c("amsync_server", class(server))
  server
}

#' Get a document from the server
#'
#' Retrieves an Automerge document by its ID.
#'
#' @param server An amsync_server object.
#' @param doc_id Document ID string.
#'
#' @return Automerge document object, or NULL if not found.
#'
#' @export
get_document <- function(server, doc_id) {
  attr(server, "sync")$documents[[doc_id]]
}

#' List all document IDs
#'
#' Returns the IDs of all documents currently loaded in the server.
#'
#' @param server An amsync_server object.
#'
#' @return Character vector of document IDs.
#'
#' @export
list_documents <- function(server) {
  ls(attr(server, "sync")$documents)
}

#' Create a new document on the server
#'
#' Creates a new empty Automerge document and registers it with the server.
#'
#' @param server An amsync_server object.
#' @param doc_id Optional document ID. If NULL, generates a new ID.
#'
#' @return Document ID string.
#'
#' @export
create_document <- function(server, doc_id = NULL) {
  state <- attr(server, "sync")

  if (is.null(doc_id)) {
    doc_id <- generate_document_id()
  }

  doc <- am_create()
  state$documents[[doc_id]] <- doc
  save_document(state, doc_id, doc)

  doc_id
}

#' Print method for amsync_server
#'
#' @param x An amsync_server object.
#' @param ... Ignored.
#'
#' @return Invisibly returns x.
#'
#' @keywords internal
#' @export
print.amsync_server <- function(x, ...) {
  state <- attr(x, "sync")
  cat("Automerge Sync Server\n")
  cat("  URL:", x$url, "\n")
  cat("  Data dir:", state$data_dir, "\n")
  cat("  Documents:", length(state$documents), "\n")
  cat("  Connections:", length(state$connections), "\n")
  NextMethod()
}
