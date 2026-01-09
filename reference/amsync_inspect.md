# Inspect a document from a sync server

Fetches a document and prints its structure for debugging.

## Usage

``` r
amsync_inspect(
  url,
  doc_id,
  timeout = 5000L,
  n = 8388608L,
  tls = NULL,
  max_depth = 2
)
```

## Arguments

- url:

  WebSocket URL of the sync server.

- doc_id:

  Document ID (base58check encoded string).

- timeout:

  Timeout in milliseconds. Default 5000.

- n:

  Maximum bytes to receive per message. Default 8388608L (8MB).

- tls:

  (optional) for secure wss:// connections to servers with self-signed
  or custom CA certificates, supply either: (i) a character path to a
  file containing the PEM-encoded TLS certificate, or (ii) a certificate
  from
  [`nanonext::write_cert()`](https://nanonext.r-lib.org/reference/write_cert.html).

- max_depth:

  Maximum depth to recurse into nested structures. Default 2.

## Value

Invisibly returns the fetched automerge document.

## Examples

``` r
if (FALSE) { # \dontrun{
# Inspect document structure from public server
amsync_inspect("wss://sync.automerge.org/", "4F63WJPDzbHkkfKa66h1Qrr1sC5U")
} # }
```
