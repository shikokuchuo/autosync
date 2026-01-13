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

An amsync_server object inheriting from 'nanoServer', with `$start()`
and `$stop()` methods.

## Details

The returned server inherits from nanonext's nanoServer class and
provides `$start()` and `$stop()` methods for non-blocking operation.

## Examples

``` r
if (FALSE) { # interactive()
# Create and start a server
server <- amsync_server()
server$start()

# Server is now running in the background
# ...do other work...

# Stop when done
server$stop()

# Custom port with TLS
cert <- nanonext::write_cert()
server <- amsync_server(port = 8080, tls = cert$server)
server$start()
server$stop()
}
```
