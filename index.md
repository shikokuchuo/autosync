# autosync

R sync server for [Automerge](https://automerge.org/) CRDT documents.
Implements the `automerge-repo` WebSocket protocol, enabling R to serve
as a synchronization hub for Automerge clients in R, JavaScript, Rust,
and other languages.

[![Ask
DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/shikokuchuo/autosync)

### Features

- Full `automerge-repo` protocol compatibility (interoperates with
  sync.automerge.org)
- Secure connections via TLS (<wss://>)
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

server <- amsync_server(port = 3030)
server$start()

# Server runs non-blocking in the background

server$close()
```

With TLS for secure connections:

``` r
cert <- nanonext::write_cert()
tls <- nanonext::tls_config(server = cert$server)

server <- amsync_server(port = 8080, tls = tls)
server$start()
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
  network: [new BrowserWebSocketClientAdapter("ws://localhost:3030")]
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

## Shiny Collaborative Editor

For a real-time collaborative text editor widget, see the
[autoedit](https://github.com/shikokuchuo/autoedit) package which
provides a CodeMirror-based editor that connects to autosync servers.

## Links

- [automerge](https://posit-dev.github.io/automerge-r/) - R bindings for
  Automerge CRDT
