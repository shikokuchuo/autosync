# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

autosync is an R package that implements a WebSocket sync server for
Automerge CRDT documents. It implements the `automerge-repo` protocol,
enabling R to serve as a synchronization hub for Automerge clients in R,
JavaScript, Rust, and other languages.

## Development Commands

``` bash
# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-server.R")

# Check package (R CMD check)
devtools::check()

# Build documentation
devtools::document()

# Install the package locally
devtools::install()
```

## Dependencies

Requires development versions of some packages: - `automerge` from
posit-dev/automerge-r - `nanonext` from <r-lib/nanonext@stream> branch

Install with: `pak::pak("shikokuchuo/autosync")`

## Architecture

### Core Components

**Server (R/server.R)**:
[`amsync_server()`](http://shikokuchuo.net/autosync/reference/amsync_server.md)
creates a WebSocket server using nanonext’s `http_server()`. The server
maintains state in environments for: - `documents` - Loaded Automerge
documents keyed by document ID - `sync_states` - Per-client,
per-document sync states (nested:
`sync_states[[client_id]][[doc_id]]`) - `connections` - WebSocket
connection objects keyed by both temp ID and client ID - `doc_peers` -
Document-to-peer mapping for broadcasting

**Handlers (R/handlers.R)**: Message routing via `handle_message()`
which dispatches to type-specific handlers: - `handle_join` - Protocol
handshake, validates version “1” - `handle_sync` - Document
synchronization using Automerge sync protocol - `handle_ephemeral` -
Transient message forwarding (point-to-point or broadcast) -
`broadcast_sync` - Propagates changes to all peers subscribed to a
document

**Client (R/client.R)**:
[`amsync_fetch()`](http://shikokuchuo.net/autosync/reference/amsync_fetch.md)
implements the client-side protocol for fetching documents from any
automerge-repo server.

**Storage (R/storage.R)**: Persistence layer using `.automerge` files in
a configurable data directory.

### Protocol Details

Messages are CBOR-encoded binary frames. Key message types: -
`join`/`peer` - Connection handshake with peer IDs and metadata -
`request`/`sync` - Document sync with Automerge sync state data -
`ephemeral` - Non-persisted messages for real-time features -
`error`/`doc-unavailable` - Error handling

Document IDs are Base58Check-encoded 16-byte random values. Peer IDs are
Base64-encoded.

### Key Imports

- `automerge` - CRDT operations (am_create, am_sync_encode/decode,
  am_save/load)
- `nanonext` - WebSocket server and async I/O
- `secretbase` - CBOR encoding (cborenc/cbordec) and Base58/Base64
- `later` - Event loop integration (run_now for async recv)

## Testing

Tests use incrementing ports starting at 4000 via `get_test_port()`
helper to avoid conflicts. Test files cover server, client, handlers,
storage, and integration scenarios.
