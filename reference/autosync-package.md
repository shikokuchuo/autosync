# autosync: 'Automerge' Sync Server and Client for R

A WebSocket-based implementation of the 'automerge-repo' synchronization
protocol used by 'sync.automerge.org'. Acts as a sync server, enabling R
to serve as a synchronization hub for 'Automerge' clients in
'JavaScript', 'Rust', and other languages, and as a client for fetching,
editing, and synchronizing documents hosted on remote servers.

## Main Functions

- [`amsync_server()`](http://shikokuchuo.net/autosync/reference/amsync_server.md):

  Create a new sync server with `$start()` and `$close()` methods

## Document Management

- [`create_document()`](http://shikokuchuo.net/autosync/reference/create_document.md):

  Create a new document

- [`get_document()`](http://shikokuchuo.net/autosync/reference/get_document.md):

  Retrieve a document by ID

- [`list_documents()`](http://shikokuchuo.net/autosync/reference/list_documents.md):

  List all document IDs

- [`generate_document_id()`](http://shikokuchuo.net/autosync/reference/generate_document_id.md):

  Generate a new document ID

## Browsing and Editing

- [`amsync_app()`](http://shikokuchuo.net/autosync/reference/amsync_app.md):

  Launch a single-window Shiny app to connect to a project, browse its
  file tree, and edit files in a live code editor

- [`amsync_project()`](http://shikokuchuo.net/autosync/reference/amsync_project.md):

  Open the files in a project document

- `$edit()`:

  The document handle from
  [`amsync_client()`](http://shikokuchuo.net/autosync/reference/amsync_client.md)'s
  `$open_doc()` edits a synced text object in a live Shiny code editor

## Protocol

The server implements the automerge-repo sync protocol over WebSockets.
Messages are CBOR-encoded and include:

- join/peer:

  Handshake messages for connection establishment

- request/sync:

  Document synchronization messages

- ephemeral:

  Transient messages forwarded without persistence

- error:

  Error notifications

## Example


    # Create and start a server
    server <- amsync_server()
    server$start()
    server$url

    # Stop when done
    server$close()

## See also

Useful links:

- <https://shikokuchuo.net/autosync/>

- <https://github.com/shikokuchuo/autosync>

- Report bugs at <https://github.com/shikokuchuo/autosync/issues>

## Author

**Maintainer**: Charlie Gao <charlie.gao@posit.co>
([ORCID](https://orcid.org/0000-0002-0750-061X))

Authors:

- Charlie Gao <charlie.gao@posit.co>
  ([ORCID](https://orcid.org/0000-0002-0750-061X))

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]
