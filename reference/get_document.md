# Get a document from the server

Retrieves an Automerge document by its ID.

## Usage

``` r
get_document(server, doc_id)
```

## Arguments

- server:

  An amsync_server object.

- doc_id:

  Document ID string.

## Value

Automerge document object, or NULL if not found.
