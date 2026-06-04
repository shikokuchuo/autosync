# Open a persistent sync connection

Connects to an automerge-repo sync server and maintains a persistent
WebSocket connection. The connection performs the protocol handshake but
holds no documents on its own; open one or more live documents over it
with the `$open_doc()` method. Each document stays synced — receiving
real-time updates from other peers and flushing local changes — for as
long as the connection is open. Unlike
[`amsync_fetch()`](http://shikokuchuo.net/autosync/reference/amsync_fetch.md),
which performs a one-off retrieval over a throwaway connection, several
documents can share a single connection here.

## Usage

``` r
amsync_client(url, timeout = 5000L, tls = NULL, token = NULL, interval = 1000L)
```

## Arguments

- url:

  WebSocket URL of the sync server (e.g., "ws://localhost:3030/" or
  "wss://sync.automerge.org/"). Note: trailing slash may be required.

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
  periodically check for and send local changes for every open document.
  This is a cheap no-op when there are no changes.

## Value

An environment of class `"amsync_client"` with reference semantics,
representing the connection:

- `open_doc(doc_id, timeout)`:

  Open a live document over this connection and return an `amsync_doc`
  handle for it (see below). Repeated calls for the same `doc_id` reuse
  the document already open on the connection rather than requesting it
  again.

- [`close()`](https://rdrr.io/r/base/connections.html):

  Disconnect and stop syncing all open documents.

- `active`:

  Logical, whether the connection is active.

An `amsync_doc` handle returned by `$open_doc()` is itself an
environment with:

- `doc`:

  The live automerge document, kept in sync with the server.

- `push()`:

  Push this document's local changes to the server immediately.

- `edit(at = "text", ext = NULL, debounce = 300L)`:

  Open this document's text object at `at` in a live Shiny code editor
  that syncs both ways, blocking until closed (requires shiny and
  bslib). `at` is a key or character-vector path to an `am_text` object,
  `ext` (e.g. `".md"`) selects syntax highlighting, and `debounce` is
  the millisecond delay before pushing keystrokes. See Details.

- [`close()`](https://rdrr.io/r/base/connections.html):

  Stop syncing this one document (detach it from the connection); the
  connection and its other documents are unaffected.

- `active`:

  Logical, whether the document is still open on an active connection.

## Details

Opening the connection performs a synchronous handshake before
returning. `$open_doc()` then performs a synchronous initial sync, so
the returned handle's `$doc` has meaningful content immediately. After
that, incoming changes are received asynchronously via a self-chaining
promise loop, and local changes are flushed periodically via a
[`later::later()`](https://later.r-lib.org/reference/later.html) timer.

Neither [`close()`](https://rdrr.io/r/base/connections.html) flushes
pending local changes. Call `$push()` first if you have unsynced edits —
otherwise any changes made since the last `sync`-interval tick may be
lost.

**Live editing.** A handle's `$edit()` opens its text object in a live
[`bslib::input_code_editor()`](https://rstudio.github.io/bslib/reference/input_code_editor.html)
that syncs both ways: as you type, the minimal diff is written to the
live document and pushed (debounced); when the text changes remotely,
the editor updates to the merged result. There is no **Save** button —
every edit is applied live. It syncs whole-text snapshots, not granular
operations, so a remote edit arriving in the brief window between a
keystroke and its debounced push can be overwritten by that push; a
smaller `debounce` narrows the window. The original's trailing-newline
state is preserved.

## Examples

``` r
if (FALSE) { # interactive()
server <- amsync_server()
server$start()
doc_id <- create_document(server)

conn <- amsync_client(server$url)
doc <- conn$open_doc(doc_id)
automerge::am_keys(doc$doc)

# Make local changes and push
automerge::am_put(doc$doc, automerge::AM_ROOT, "key", "value")
doc$push()

# Open another document over the same connection
other <- conn$open_doc(create_document(server))

# Disconnect (closes every document on the connection)
conn$close()
server$close()
}
```
