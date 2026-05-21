# Create a persistent sync client

Connects to an automerge-repo sync server and maintains a persistent
WebSocket connection for continuous document synchronization. Unlike
[`amsync_fetch()`](http://shikokuchuo.net/autosync/reference/amsync_fetch.md),
which performs a one-off retrieval, this client stays connected and
receives real-time updates from other peers.

## Usage

``` r
amsync_client(
  url,
  doc_id,
  timeout = 5000L,
  tls = NULL,
  token = NULL,
  interval = 1000L
)
```

## Arguments

- url:

  WebSocket URL of the sync server (e.g., "ws://localhost:3030/" or
  "wss://sync.automerge.org/"). Note: trailing slash may be required.

- doc_id:

  Document ID (base58check encoded string)

- timeout:

  Timeout in milliseconds for each receive operation. Default 5000.

- tls:

  (optional) for secure wss:// connections to servers with self-signed
  or custom CA certificates, a TLS configuration object created by
  [`nanonext::tls_config()`](https://nanonext.r-lib.org/reference/tls_config.html).

- token:

  (optional) JWT (ID token) for authenticated servers. Sent as a Bearer
  token in the Authorization header of the WebSocket upgrade request.

- interval:

  Interval in milliseconds for pushing local changes to the server.
  Default 1000. Uses
  [`later::later()`](https://later.r-lib.org/reference/later.html) to
  periodically check for and send local changes. This is a cheap no-op
  when there are no changes.

## Value

An environment of class `"amsync_client"` with reference semantics,
containing:

- `doc`:

  The live automerge document, kept in sync with the server.

- `push()`:

  Push local changes to the server immediately.

- [`close()`](https://rdrr.io/r/base/connections.html):

  Disconnect and stop syncing.

- `active`:

  Logical, whether the connection is active.

## Details

The client performs a synchronous handshake and initial sync before
returning, so `$doc` has meaningful content immediately. After that,
incoming changes are received asynchronously via a self-chaining promise
loop, and local changes are flushed periodically via a
[`later::later()`](https://later.r-lib.org/reference/later.html) timer.

`$close()` does not flush pending local changes. Call `$push()` first if
you have unsynced edits — otherwise any changes made since the last
`sync`-interval tick may be lost.

## Examples

``` r
if (FALSE) { # interactive()
server <- amsync_server()
server$start()
doc_id <- create_document(server)

client <- amsync_client(server$url, doc_id)
automerge::am_keys(client$doc)

# Make local changes and push
automerge::am_put(client$doc, automerge::AM_ROOT, "key", "value")
client$push()

# Disconnect
client$close()
server$close()
}
```
