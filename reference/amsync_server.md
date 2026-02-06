# Create an Automerge sync server

Creates a WebSocket server that implements the automerge-repo sync
protocol, compatible with JavaScript, Rust, and other Automerge clients.

## Usage

``` r
amsync_server(
  port = 3030L,
  host = "127.0.0.1",
  data_dir = ".automerge",
  auto_create_docs = TRUE,
  storage_id = NULL,
  tls = NULL,
  auth = NULL
)
```

## Arguments

- port:

  Port to listen on. Default 3030.

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
  When provided, clients must include a valid OAuth2 access token in
  their join message's peerMetadata. Requires the gargle package. Note:
  TLS is required when authentication is enabled to protect tokens.

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

# Custom port with TLS
cert <- nanonext::write_cert()
tls <- nanonext::tls_config(server = cert$server)
server <- amsync_server(port = 8080, tls = tls)
server$start()
server$close()

# Server with Google OAuth authentication (requires TLS)
cert <- nanonext::write_cert()
tls <- nanonext::tls_config(server = cert$server)
server <- amsync_server(
  port = 3030,
  tls = tls,
  auth = auth_config(allowed_domains = c("mycompany.com"))
)
}
```
