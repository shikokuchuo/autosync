# autosync: Automerge Sync Server for R

A WebSocket-based synchronization server for Automerge documents,
compatible with the automerge-repo protocol used by sync.automerge.org.
Enables R to serve as a sync hub for JavaScript, Rust, and other
Automerge clients in collaborative applications.

## Main Functions

- [`amsync_server()`](http://shikokuchuo.net/autosync/reference/amsync_server.md):

  Create a new sync server

- [`serve()`](http://shikokuchuo.net/autosync/reference/serve.md):

  Start the server (blocking)

- [`stop_server()`](http://shikokuchuo.net/autosync/reference/stop_server.md):

  Stop a running server

## Document Management

- [`create_document()`](http://shikokuchuo.net/autosync/reference/create_document.md):

  Create a new document

- [`get_document()`](http://shikokuchuo.net/autosync/reference/get_document.md):

  Retrieve a document by ID

- [`list_documents()`](http://shikokuchuo.net/autosync/reference/list_documents.md):

  List all document IDs

- [`generate_document_id()`](http://shikokuchuo.net/autosync/reference/generate_document_id.md):

  Generate a new document ID

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
    server <- amsync_server(port = 3030)
    serve(server)  # Blocks until stopped

## See also

Useful links:

- <https://github.com/shikokuchuo/autosync>

- <http://shikokuchuo.net/autosync/>

- Report bugs at <https://github.com/shikokuchuo/autosync/issues>

## Author

**Maintainer**: Charlie Gao <charlie.gao@posit.co>
([ORCID](https://orcid.org/0000-0002-0750-061X))

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]
