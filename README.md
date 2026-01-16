
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

## Shiny Collaborative Editor

A real-time collaborative text editor widget for Shiny applications:

``` r
library(shiny)
library(automerge)
library(autosync)

server <- amsync_server(port = 3030, host = "127.0.0.1")
server$start()

doc_id <- create_document(server)
doc <- get_document(server, doc_id)
am_put(doc, AM_ROOT, "text", am_text(""))
am_commit(doc, "init")

ui <- fluidPage(autosync_editor_output("editor"))

shiny_server <- function(input, output, session) {
  output$editor <- render_autosync_editor(autosync_editor(server$url, doc_id))
}

onStop(function() server$stop())
shinyApp(ui, shiny_server)
```

Open in multiple browser windows to see real-time collaboration.

## Links

Documentation: <https://shikokuchuo.net/autosync/>
