# Open files from a project document

Given a sync server URL and a project document ID, opens a persistent
connection to the server, syncs the project document over it, and
exposes its file tree for opening individual files. A project is an
Automerge document with a `files` map whose keys are file paths and
whose values are text objects holding each file's own document ID.

## Usage

``` r
amsync_project(
  url,
  proj_id,
  token = NULL,
  tls = NULL,
  timeout = 5000L,
  files_key = "files"
)
```

## Arguments

- url:

  WebSocket URL of the sync server (e.g., "ws://localhost:3030/" or
  "wss://sync.automerge.org/"). Note: trailing slash may be required.

- proj_id:

  Document ID of the project.

- token:

  (optional) JWT (ID token) for authenticated servers. Sent as a Bearer
  token in the Authorization header of the WebSocket upgrade request.

- tls:

  (optional) for secure wss:// connections to servers with self-signed
  or custom CA certificates, a TLS configuration object created by
  [`nanonext::tls_config()`](https://nanonext.r-lib.org/reference/tls_config.html).

- timeout:

  Timeout in milliseconds for each receive operation. Default 5000.

- files_key:

  Key of the files map within the project document. Default `"files"`.

## Value

An environment of class `"amsync_project"` (reference semantics) with
the following fields and methods:

- `doc`:

  The live project document, kept in sync with the server.

- `conn`:

  The underlying
  [`amsync_client()`](http://shikokuchuo.net/autosync/reference/amsync_client.md)
  connection.

- `paths()`:

  Current sorted file paths.

- `doc_id(path)`:

  Resolve a path to its document ID.

- `open(path)`:

  Open the file's document over the project connection and return its
  `amsync_doc` handle. Reuses the connection and any already-open
  document.

- `refresh()`:

  Re-resolve the file tree to pick up added or removed files (the
  project document syncs live, so this just settles pending updates).

- [`close()`](https://rdrr.io/r/base/connections.html):

  Disconnect the project connection.

## Details

Opening a file syncs that file's document over the **same** connection
rather than dialing the server again, so a session reuses a single
WebSocket throughout. Call the opened file's `$edit()` method to edit it
live, or use
[`amsync_app()`](http://shikokuchuo.net/autosync/reference/amsync_app.md)
for an interactive browser. Call `$close()` when finished to disconnect.

## Examples

``` r
if (FALSE) { # interactive()
proj <- amsync_project("wss://quarto-hub.com/ws", proj_id, token = amsync_token())
proj                                   # prints the file tree
doc <- proj$open("/charlie/index.qmd") # open a file over the connection
doc$edit(at = "text", ext = ".qmd")    # edit it live
proj$close()                           # disconnect when finished
}
```
