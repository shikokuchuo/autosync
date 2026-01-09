# Create an Automerge sync server

Creates a WebSocket server that implements the automerge-repo sync
protocol, compatible with JavaScript, Rust, and other Automerge clients.

## Usage

``` r
amsync_server(
  port = 3030L,
  host = "0.0.0.0",
  data_dir = ".amrg",
  auto_create_docs = TRUE,
  storage_id = NULL,
  tls = NULL
)
```

## Arguments

- port:

  Port to listen on. Default 3030.

- host:

  Host address to bind to. Default "0.0.0.0" (all interfaces).

- data_dir:

  Directory for persistent document storage. Default ".amrg".

- auto_create_docs:

  Logical, whether to auto-create documents when clients request unknown
  document IDs. Default TRUE.

- storage_id:

  Optional storage ID for this server. If NULL (default), generates a
  new persistent identity. Set to NA for an ephemeral server (no
  persistence identity).

- tls:

  (optional) for secure wss:// connections, supply either: (i) a
  character path to a file containing the PEM-encoded TLS certificate
  and associated private key, or (ii) a length-2 character vector of
  [`nanonext::write_cert()`](https://nanonext.r-lib.org/reference/write_cert.html)
  comprising the certificate followed by the private key.

## Value

An amsync_server object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic ws:// server on default port
server <- amsync_server()
serve(server)

# Create server on custom port with specific data directory
server <- amsync_server(port = 8080, data_dir = "my_docs")
serve(server)

# Secure wss:// server with auto-generated certificate
server <- amsync_server(port = 3030, tls = nanonext::write_cert()$server)
serve(server)

# Secure wss:// server with certificate file
server <- amsync_server(port = 443, tls = "/etc/ssl/private/server.pem")
serve(server)
} # }
```
