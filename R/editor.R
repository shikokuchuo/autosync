# Collaborative text editor widget

#' Collaborative text editor widget
#'
#' Creates a real-time collaborative text editor using CodeMirror 6 and
#' Automerge CRDT, synchronized via WebSocket to an autosync server.
#'
#' @param server_url WebSocket URL (ws:// or wss://) of the autosync server.
#' @param doc_id Document ID (base58check encoded).
#' @param timeout Connection timeout in milliseconds. Default 10000.
#' @param width,height Widget dimensions.
#'
#' @return An htmlwidget object.
#'
#' @details
#' The widget connects to the specified autosync server and synchronizes
#' a CodeMirror 6 editor with an Automerge document. Multiple users can
#' edit the same document simultaneously with automatic conflict resolution.
#'
#' The Automerge document must have a "text" field of type Automerge text
#' (created with `automerge::am_text()`).
#'
#' In Shiny applications, the current editor content is available as an

#' input value at `input$<outputId>_content`.
#'
#' @examples
#' \dontrun{
#' # Basic usage with an autosync server
#' server <- amsync_server(port = 3030)
#' server$start()
#'
#' doc_id <- create_document(server)
#' doc <- get_document(server, doc_id)
#' automerge::am_put(doc, automerge::AM_ROOT, "text", automerge::am_text(""))
#' automerge::am_commit(doc, "init")
#'
#' autosync_editor(server$url, doc_id)
#' }
#'
#' @export
autosync_editor <- function(server_url, doc_id, timeout = 10000,
                            width = "100%", height = "400px") {
  rlang::check_installed("htmlwidgets")
  htmlwidgets::createWidget(
    name = "autosyncEditor",
    x = list(serverUrl = server_url, docId = doc_id, timeout = timeout),
    width = width,
    height = height,
    package = "autosync"
  )
}

#' Shiny bindings for autosync_editor
#'
#' Output and render functions for using autosync_editor within Shiny
#' applications and interactive R Markdown documents.
#'
#' @param outputId Output variable to read from.
#' @param width,height Widget dimensions (must be valid CSS unit or a number
#'   which will be coerced to a string and have "px" appended).
#' @param expr An expression that generates an autosync_editor widget.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Logical, whether `expr` is a quoted expression.
#'
#' @return `autosync_editor_output()` returns a Shiny output element.
#'   `render_autosync_editor()` returns a Shiny render function.
#'
#' @name autosync_editor-shiny
#'
#' @export
autosync_editor_output <- function(outputId, width = "100%", height = "400px") {
  rlang::check_installed("htmlwidgets")
  htmlwidgets::shinyWidgetOutput(outputId, "autosyncEditor", width, height,
                                  package = "autosync")
}

#' @rdname autosync_editor-shiny
#' @export
render_autosync_editor <- function(expr, env = parent.frame(), quoted = FALSE) {
  rlang::check_installed("htmlwidgets")
  if (!quoted) expr <- substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, autosync_editor_output, env, quoted = TRUE)
}
