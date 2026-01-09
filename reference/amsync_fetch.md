# Fetch a document from a sync server

Connects to an automerge-repo sync server and retrieves a document by
ID. This is useful for debugging sync issues or fetching documents from
remote servers like sync.automerge.org.

## Usage

``` r
amsync_fetch(url, doc_id, timeout = 5000L, tls = NULL, verbose = FALSE)
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
  or custom CA certificates, supply either: (i) a character path to a
  file containing the PEM-encoded TLS certificate, or (ii) a certificate
  from
  [`nanonext::write_cert()`](https://nanonext.r-lib.org/reference/write_cert.html).

- verbose:

  Logical, print debug messages. Default FALSE.

## Value

An automerge document object containing the fetched data.

## Details

The function implements the automerge-repo sync protocol:

1.  Connects via WebSocket

2.  Sends join message with peer ID

3.  Receives peer response from server

4.  Sends sync request for the specified document

5.  Receives and applies sync messages until complete

Sync is considered complete when no new messages arrive within the
timeout after at least one sync round.

## Examples

``` r
if (FALSE) { # \dontrun{
# Fetch from public sync server
doc <- amsync_fetch("wss://sync.automerge.org", "4F63WJPDzbHkkfKa66h1Qrr1sC5U")

# Fetch from local server with debug output
doc <- amsync_fetch("ws://localhost:3030", "myDocId", verbose = TRUE)

# Fetch from server with self-signed certificate
doc <- amsync_fetch("wss://localhost:3030", "myDocId",
                    tls = nanonext::write_cert()$client)

# Inspect the document
automerge::am_keys(doc)
} # }
```
