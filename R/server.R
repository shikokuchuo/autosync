# Automerge sync server implementation using nanonext

#' Create an Automerge sync server
#'
#' Creates a WebSocket server that implements the automerge-repo sync protocol,
#' compatible with JavaScript, Rust, and other Automerge clients.
#'
#' @param port Port to listen on. Default 3030.
#' @param host Host address to bind to. Default "0.0.0.0" (all interfaces).
#' @param data_dir Directory for persistent document storage. Default ".amrg".
#' @param auto_create_docs Logical, whether to auto-create documents when
#'   clients request unknown document IDs. Default TRUE.
#' @param storage_id Optional storage ID for this server. If NULL (default),
#'   generates a new persistent identity. Set to NA for an ephemeral server
#'   (no persistence identity).
#' @param tls (optional) for secure wss:// connections, supply either: (i) a
#'   character path to a file containing the PEM-encoded TLS certificate and
#'   associated private key, or (ii) a length-2 character vector of
#'   [nanonext::write_cert()] comprising the certificate followed by the
#'   private key.
#'
#' @return An amsync_server object.
#'
#' @examples
#' \dontrun{
#' # Basic ws:// server on default port
#' server <- amsync_server()
#' serve(server)
#'
#' # Create server on custom port with specific data directory
#' server <- amsync_server(port = 8080, data_dir = "my_docs")
#' serve(server)
#'
#' # Secure wss:// server with auto-generated certificate
#' server <- amsync_server(port = 3030, tls = nanonext::write_cert()$server)
#' serve(server)
#'
#' # Secure wss:// server with certificate file
#' server <- amsync_server(port = 443, tls = "/etc/ssl/private/server.pem")
#' serve(server)
#' }
#'
#' @export
amsync_server <- function(
  port = 3030L,
  host = "0.0.0.0",
  data_dir = ".amrg",
  auto_create_docs = TRUE,
  storage_id = NULL,
  tls = NULL
) {
  server <- new.env(hash = TRUE, parent = emptyenv())
  server$port <- as.integer(port)
  server$host <- host
  server$data_dir <- data_dir
  server$auto_create_docs <- auto_create_docs
  server$tls <- if (!is.null(tls)) tls_config(server = tls)

  scheme <- if (is.null(tls)) "http" else "https"
  server$url <- sprintf("%s://%s:%d", scheme, host, port)

  server$peer_id <- generate_peer_id()
  server$storage_id <- if (is.null(storage_id)) {
    generate_peer_id()
  } else if (is.na(storage_id)) {
    NULL # Ephemeral server
  } else {
    storage_id # User-provided
  }

  server$documents <- new.env(hash = TRUE, parent = emptyenv())
  server$sync_states <- new.env(hash = TRUE, parent = emptyenv())
  server$connections <- new.env(hash = TRUE, parent = emptyenv())
  server$running <- FALSE
  server$nano_server <- NULL

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  class(server) <- "amsync_server"
  server
}

#' Start the sync server (blocking)
#'
#' Starts the WebSocket server and enters the event loop. This function
#' blocks until the server is stopped via [stop_server()].
#'
#' @param server An amsync_server object.
#'
#' @return Invisibly returns the server object.
#'
#' @examples
#' \dontrun{
#' server <- amsync_server()
#' serve(server)  # Blocks until stopped
#' }
#'
#' @export
serve <- function(server) {
  if (!inherits(server, "amsync_server")) {
    stop("'server' must be an amsync_server object")
  }

  server$running <- TRUE
  load_all_documents(server)

  on_open <- function(ws) {
    ws_id <- as.character(ws$id)
    server$connections[[ws_id]] <- list(
      ws = ws,
      client_id = NULL,
      metadata = NULL,
      connected_at = Sys.time()
    )
  }

  on_message <- function(ws, data) {
    ws_id <- as.character(ws$id)
    conn <- server$connections[[ws_id]]
    client_id <- if (!is.null(conn$client_id)) conn$client_id else ws_id
    handle_message(server, client_id, ws_id, data)
  }

  on_close <- function(ws) {
    ws_id <- as.character(ws$id)
    conn <- server$connections[[ws_id]]
    if (!is.null(conn)) {
      client_id <- conn$client_id
      handle_disconnect(server, client_id)
      rm(list = ws_id, envir = server$connections)
      if (
        !is.null(client_id) && exists(client_id, envir = server$connections)
      ) {
        rm(list = client_id, envir = server$connections)
      }
    }
  }

  server$nano_server <- nanonext::http_server(
    url = server$url,
    handlers = list(),
    ws_path = "/",
    on_open = on_open,
    on_message = on_message,
    on_close = on_close,
    tls = server$tls,
    textframes = FALSE
  )
  server$nano_server$start()

  ws_scheme <- if (is.null(server$tls)) "ws" else "wss"
  cat(
    "Autosync server running on ",
    ws_scheme,
    "://",
    server$host,
    ":",
    server$port,
    "\n",
    sep = ""
  )
  cat("Press Ctrl+C to stop\n")

  tryCatch(
    {
      while (server$running) {
        later::run_now(timeoutSecs = Inf)
      }
    },
    interrupt = function(e) {
      message("\nShutting down...")
    }
  )

  stop_server(server)
  invisible(server)
}

#' Stop the sync server
#'
#' Stops a running sync server and cleans up resources.
#'
#' @param server An amsync_server object.
#'
#' @return Invisibly returns the server object.
#'
#' @export
stop_server <- function(server) {
  if (!inherits(server, "amsync_server")) {
    stop("'server' must be an amsync_server object")
  }
  server$running <- FALSE
  if (!is.null(server$nano_server)) {
    server$nano_server$close()
    server$nano_server <- NULL
  }
  invisible(server)
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
  if (!inherits(server, "amsync_server")) {
    stop("'server' must be an amsync_server object")
  }
  server$documents[[doc_id]]
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
  if (!inherits(server, "amsync_server")) {
    stop("'server' must be an amsync_server object")
  }
  ls(server$documents)
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
  if (!inherits(server, "amsync_server")) {
    stop("'server' must be an amsync_server object")
  }

  if (is.null(doc_id)) {
    doc_id <- generate_document_id()
  }

  doc <- am_create()
  server$documents[[doc_id]] <- doc
  save_document(server, doc_id, doc)

  doc_id
}

#' Print method for amsync_server
#'
#' @param x An amsync_server object.
#' @param ... Ignored.
#'
#' @return Invisibly returns x.
#'
#' @export
print.amsync_server <- function(x, ...) {
  cat("Automerge Sync Server\n")
  cat("  Host:", x$host, "\n")
  cat("  Port:", x$port, "\n")
  cat("  Data dir:", x$data_dir, "\n")
  cat("  Running:", x$running, "\n")
  cat("  Documents:", length(ls(x$documents)), "\n")
  cat("  Connections:", length(ls(x$connections)), "\n")
  invisible(x)
}
