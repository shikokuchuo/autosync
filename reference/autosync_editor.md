# Collaborative text editor widget

Creates a real-time collaborative text editor using CodeMirror 6 and
Automerge CRDT, synchronized via WebSocket to an autosync server.

## Usage

``` r
autosync_editor(
  server_url,
  doc_id,
  timeout = 10000,
  width = "100%",
  height = "400px"
)
```

## Arguments

- server_url:

  WebSocket URL (ws:// or wss://) of the autosync server.

- doc_id:

  Document ID (base58check encoded).

- timeout:

  Connection timeout in milliseconds. Default 10000.

- width, height:

  Widget dimensions.

## Value

An htmlwidget object.

## Details

The widget connects to the specified autosync server and synchronizes a
CodeMirror 6 editor with an Automerge document. Multiple users can edit
the same document simultaneously with automatic conflict resolution.

The Automerge document must have a "text" field of type Automerge text
(created with
[`automerge::am_text()`](https://posit-dev.github.io/automerge-r/reference/am_text.html)).

In Shiny applications, the current editor content is available as an
input value at `input$<outputId>_content`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with an autosync server
server <- amsync_server(port = 3030)
server$start()

doc_id <- create_document(server)
doc <- get_document(server, doc_id)
automerge::am_put(doc, automerge::AM_ROOT, "text", automerge::am_text(""))
automerge::am_commit(doc, "init")

autosync_editor(server$url, doc_id)
} # }
```
