# autosync

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/shikokuchuo/autosync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shikokuchuo/autosync/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/shikokuchuo/autosync/graph/badge.svg)](https://app.codecov.io/gh/shikokuchuo/autosync)
<!-- badges: end -->

R sync server for [Automerge](https://automerge.org/) CRDT documents. Implements the `automerge-repo` WebSocket protocol, enabling R to serve as a synchronization hub for Automerge clients in R, JavaScript, Rust, and other languages.

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/shikokuchuo/autosync)

### Features

- Full `automerge-repo` protocol compatibility (interoperates with sync.automerge.org)
- Secure connections via TLS (wss://)
- Persistent document storage
- Ephemeral message broadcasting
- Client functions for fetching documents from remote servers

## Installation

``` r
pak::pak("shikokuchuo/autosync")
```

## Server

Create a WebSocket sync server:

``` r
library(autosync)

server <- amsync_server()
server$start()
server

# Server runs non-blocking in the background

server$close()
```

With TLS for secure connections:

``` r
cert <- nanonext::write_cert()
tls <- nanonext::tls_config(server = cert$server)

server <- amsync_server(tls = tls)
server$start()
server$url
```

### Authentication

Enable Google OAuth2 authentication to restrict access:

``` r
cert <- nanonext::write_cert()
tls <- nanonext::tls_config(server = cert$server)

server <- amsync_server(
  tls = tls,  # TLS required when auth is enabled
  auth = auth_config(allowed_domains = "posit.co")
)
server$start()
```

Clients must provide an access token:

``` r
token <- amsync_auth()  # Interactive OAuth flow
tlsclient <- nanonext::tls_config(client = cert$client)
doc <- amsync_fetch(
  server$url,
  "35ei6ouA7nLhtjmf3d9xk1KKvtKv",
  access_token = token,
  tls = tlsclient
)
```

### Server peering

Servers can peer to sync documents bidirectionally. Documents on one server are automatically announced and synced to its peer:

``` r
# Start server A and create a document
server_a <- amsync_server(data_dir = tempfile())
server_a$start()
doc_id <- create_document(server_a)

# Start server B, peering with A
server_b <- amsync_server(data_dir = tempfile(), peer = server_a$url)
server_b$start()

# Allow the peer handshake and sync to complete
for (i in 1:10) later::run_now(1) || break

# The document has synced from A to B
list_documents(server_b)

server_a$close()
server_b$close()
```

### Document management

``` r
# Create a new document
doc_id <- create_document(server)

# List all documents
list_documents(server)

# Get a document
doc <- get_document(server, doc_id)
```

### Connecting from JavaScript

``` javascript
import { Repo } from "@automerge/automerge-repo"
import { BrowserWebSocketClientAdapter } from "@automerge/automerge-repo-network-websocket"

const repo = new Repo({
  network: [new BrowserWebSocketClientAdapter("ws://localhost:8080")]
})
```

## Client

Fetch documents from any automerge-repo sync server:

``` r
# Fetch from public sync server
doc <- amsync_fetch("wss://sync.automerge.org", "your-document-id")

# Inspect document structure
str(doc)

# Or verbose fetch for debugging sync issues
doc <- amsync_fetch("wss://sync.automerge.org", "your-document-id", verbose = TRUE)
```

## Utilities

Generate document IDs compatible with automerge-repo:

``` r
doc_id <- generate_document_id()
```

## Links

- [automerge](https://posit-dev.github.io/automerge-r/) - R bindings for Automerge CRDT
