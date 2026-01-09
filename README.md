
# autosync

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/shikokuchuo/autosync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shikokuchuo/autosync/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

R sync server for [Automerge](https://automerge.org/) CRDT documents.
Implements the automerge-repo WebSocket protocol, enabling R to serve as
a synchronization hub for Automerge clients (in R, JavaScript, Rust,
etc.).

### Features

- Full automerge-repo protocol compatibility (interoperates with
  sync.automerge.org)
- Secure connections via TLS (<wss://>)
- Persistent document storage
- Client functions for fetching documents from remote servers

## Installation

``` r
pak::pak("shikokuchuo/autosync")
```

## Server

``` r
library(autosync)

# Start a sync server
server <- amsync_server(port = 3030)
serve(server)  # Blocks until Ctrl+C
```

Connect from JavaScript:

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
automerge::am_keys(doc)
```

## Links

Documentation: <https://shikokuchuo.net/autosync/>
