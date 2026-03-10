# Create an Automerge sync server

Creates a WebSocket server that implements the automerge-repo sync
protocol, compatible with JavaScript, Rust, and other Automerge clients.

## Usage

``` r
amsync_server(
  port = 0L,
  host = "127.0.0.1",
  data_dir = ".automerge",
  auto_create_docs = TRUE,
  storage_id = NULL,
  tls = NULL,
  auth = NULL,
  share = NA
)
```

## Arguments

- port:

  Port to listen on. Default 0 (binds to a random available port). The
  actual URL is retrieved via `server$url`.

- host:

  Host address to bind to. Default "127.0.0.1" (localhost).

- data_dir:

  Directory for persistent document storage. Default ".automerge".

- auto_create_docs:

  Logical, whether to auto-create documents when clients request unknown
  document IDs. Default TRUE.

- storage_id:

  Optional storage ID for this server. If NULL (default), generates a
  new persistent identity. Set to NA for an ephemeral server (no
  persistence identity).

- tls:

  (optional) for secure wss:// connections, a TLS configuration object
  created by
  [`nanonext::tls_config()`](https://nanonext.r-lib.org/reference/tls_config.html).

- auth:

  Optional authentication configuration created by
  [`auth_config()`](http://shikokuchuo.net/autosync/reference/auth_config.md).
  When provided, clients must include a valid JWT (ID token) as a Bearer
  token in the Authorization header of the WebSocket upgrade request.
  Connections without valid credentials are rejected immediately. Note:
  TLS is required when authentication is enabled to protect tokens.

- share:

  Controls document sharing policy for connected clients. This unified
  parameter governs both proactive document announcement (pushing
  documents to clients) and access control (allowing clients to request
  documents). Accepts one of:

  - `NA` (default) — never announce but allow all requests.

  - `TRUE` — announce all documents to all clients and allow all
    requests.

  - `FALSE` — never announce and deny all requests (sends
    `doc-unavailable`).

  - A function with signature `function(client_id, doc_id)` returning
    `TRUE` (announce and allow), `NA` (allow on request only), or
    `FALSE` (deny access). Called per client and per document.

## Value

An amsync_server object inheriting from 'nanoServer', with `$start()`
and `$close()` methods.

## Details

The returned server inherits from nanonext's nanoServer class and
provides `$start()` and `$close()` methods for non-blocking operation.

## Examples

``` r
if (FALSE) { # interactive()
# Create and start a server
server <- amsync_server()
server$start()

# Server is now running in the background
# ...do other work...

# Stop when done
server$close()

# With TLS for secure connections
cert <- nanonext::write_cert()
tls <- nanonext::tls_config(server = cert$server)
server <- amsync_server(tls = tls)
server$start()
server$url
server$close()

# Server with OIDC authentication (requires TLS)
cert <- nanonext::write_cert()
tls <- nanonext::tls_config(server = cert$server)
server <- amsync_server(
  tls = tls,
  auth = auth_config(
    client_id = "123456789.apps.googleusercontent.com",
    allowed_domains = "mycompany.com"
  )
)
}
```
