# Single-window Shiny entry point: connect, browse, and edit in one app

#' Launch the autosync browser app
#'
#' Opens a single Shiny app that carries the whole workflow from start to finish
#' without any other R commands. It has two screens served in one window:
#'
#' * **Connect** -- enter a sync-server URL and a project document ID, and
#'   optionally authenticate. The **Authenticate** button runs the same OIDC
#'   browser flow as [amsync_token()]; client ID, secret, and issuer can be set
#'   under **Advanced** (prefilled from the `OIDC_CLIENT_ID`,
#'   `OIDC_CLIENT_SECRET`, and `OIDC_ISSUER` environment variables). Passing a
#'   `token` obtained earlier from [amsync_token()] starts the app already
#'   signed in, skipping that step. Leaving the sign-in untouched connects
#'   without a token, for open servers.
#' * **Browse & edit** -- once connected, the project's file tree appears in a
#'   sidebar; selecting a file opens its document in a live
#'   [bslib::input_code_editor()] that stays in sync with the server in both
#'   directions, just like a document handle's `$edit()` method. **Disconnect**
#'   returns to the connect screen; closing the window ends the session.
#'
#' This is a front door to [amsync_project()]: it builds the same connection and
#' reuses it for every file opened during the session.
#'
#' @inheritParams amsync_project
#' @param server Initial sync-server URL to prefill in the connect form.
#'   Default `""`.
#' @param proj_id Initial project document ID to prefill. Default `""`.
#' @param token (optional) A JWT obtained earlier from [amsync_token()]. When
#'   supplied, the app starts already signed in; you can still re-authenticate
#'   from the form. Default `NULL` (sign in from the form, or connect with no
#'   token).
#' @param debounce Milliseconds to debounce outgoing editor changes, passed
#'   through to the live editor. Default 300.
#'
#' @return Invisibly `NULL`, when the app window is closed.
#'
#' @details
#' Requires the \pkg{shiny} and \pkg{bslib} packages and an interactive session.
#'
#' @examplesIf interactive()
#' # Start with empty fields and fill them in the form:
#' amsync_app()
#'
#' # Or prefill the server and project so only sign-in/Connect remain:
#' amsync_app("wss://quarto-hub.com/ws", proj_id = "4F63WJPDzbHkkfKa66h1Qrr1sC5U")
#'
#' # Reuse a token obtained earlier, so the app starts signed in:
#' token <- amsync_token()
#' amsync_app("wss://quarto-hub.com/ws", proj_id = "4F63WJPD...", token = token)
#'
#' @importFrom automerge am_text_content
#' @export
amsync_app <- function(
  server = "",
  proj_id = "",
  token = NULL,
  tls = NULL,
  timeout = 5000L,
  files_key = "files",
  debounce = 300L
) {
  if (!is_interactive()) {
    stop("`amsync_app()` requires an interactive session")
  }
  if (
    !requireNamespace("shiny", quietly = TRUE) ||
      !requireNamespace("bslib", quietly = TRUE)
  ) {
    stop(
      "amsync_app() requires the 'shiny' and 'bslib' packages.\n",
      'Install them with install.packages(c("shiny", "bslib")).'
    )
  }
  if (
    !is.null(token) &&
      (!is.character(token) || length(token) != 1L || is.na(token) ||
        !nzchar(token))
  ) {
    stop("`token` must be a single non-empty string (from `amsync_token()`), or NULL")
  }

  app <- build_amsync_app(server, proj_id, token, tls, timeout, files_key, debounce)
  shiny::runGadget(app, stopOnCancel = FALSE)
  invisible(NULL)
}

#' Build the autosync browser Shiny app object
#'
#' Splits the app's UI and server out of [amsync_app()] so the same app can be
#' launched as a gadget there and driven by `shiny::testServer()` in tests. The
#' parameters mirror [amsync_app()].
#'
#' @return A [shiny::shinyApp()] object.
#'
#' @noRd
build_amsync_app <- function(
  server,
  proj_id,
  token,
  tls,
  timeout,
  files_key,
  debounce
) {
  ui <- bslib::page_fillable(
    title = "amsync",
    padding = 0,
    shiny::uiOutput("screen", fill = TRUE)
  )

  # Named `app_server` rather than `server` so it does not shadow the `server`
  # argument (the prefill URL), which the connect screen reads when rendering.
  app_server <- function(input, output, session) {
    # Connection and editor state lives in a plain environment, not a reactive
    # one, so the sync observers can read the live document without taking a
    # reactive dependency on it (which would re-fire them on every edit). Only
    # the screen and the currently-open path are reactive. install_editor_sync()
    # reads $doc/$at/$base/$shown from here, just as it does in edit_in_shiny().
    st <- new.env(parent = emptyenv())
    st$proj <- NULL # the amsync_project connection, once connected
    st$token <- token # JWT, pre-supplied or from the Authenticate flow
    st$doc <- NULL # the currently-open amsync_doc handle
    st$at <- "text" # path to the text object within a file document
    st$base <- "" # the open file's content (for trailing-newline state)
    st$shown <- "" # content the editor and document last agreed on

    rv <- shiny::reactiveValues(
      view = "connect", # "connect" or "browse"
      authed = !is.null(token), # whether a token has been obtained
      paths = character(0), # file paths shown in the tree (re-render on refresh)
      selected = NULL # the open file path, or NULL for none
    )

    # --- Screen switching: connect form vs browse/edit layout ---

    output$screen <- shiny::renderUI({
      if (identical(rv$view, "closed")) {
        closed_screen_ui()
      } else if (identical(rv$view, "connect")) {
        shiny::div(
          class = paste(
            "d-flex h-100 w-100 justify-content-center",
            "align-items-start overflow-auto p-3"
          ),
          connect_screen_ui(server, proj_id)
        )
      } else {
        browse_screen_ui()
      }
    })

    # The file tree re-renders only when the path set changes (connect /
    # refresh), not on selection -- the active-row highlight is handled
    # client-side, so we read the current selection with isolate() to restore it
    # after a refresh without taking a reactive dependency on it.
    output$filetree <- shiny::renderUI({
      build_file_tree_ui(rv$paths, shiny::isolate(rv$selected))
    })

    output$auth_status <- shiny::renderUI(auth_status_ui(rv$authed))

    # --- Connect screen: authenticate ---

    # amsync_token() drives the shared event loop with run_now() while it waits
    # for the OAuth callback; run_now() is reentrant-safe, so calling it from
    # within this observer is fine. The browser opening is the user's feedback.
    shiny::observeEvent(input$authenticate, {
      issuer <- trimws(input$issuer %||% "")
      token <- tryCatch(
        amsync_token(
          client_id = trimws(input$client_id %||% ""),
          client_secret = input$client_secret %||% "",
          issuer = if (nzchar(issuer)) issuer else oidc_issuer()
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Authentication failed:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      if (!is.null(token)) {
        st$token <- token
        rv$authed <- TRUE
        shiny::showNotification("Signed in.", type = "message")
      }
    })

    # --- Connect screen: connect and switch to browsing ---

    shiny::observeEvent(input$connect, {
      url_in <- trimws(input$url %||% "")
      proj_in <- trimws(input$proj_id %||% "")
      if (!nzchar(url_in) || !nzchar(proj_in)) {
        shiny::showNotification(
          "Enter both a server URL and a project ID.",
          type = "warning"
        )
        return()
      }
      proj <- tryCatch(
        amsync_project(
          url_in,
          proj_in,
          token = st$token,
          tls = tls,
          timeout = timeout,
          files_key = files_key
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Connection failed:", conditionMessage(e)),
            type = "error"
          )
          NULL
        }
      )
      if (is.null(proj)) {
        return()
      }
      st$proj <- proj
      st$doc <- NULL
      rv$paths <- proj$paths()
      rv$selected <- NULL
      rv$view <- "browse"
    })

    # --- Browse screen: open the selected file in the editor ---

    shiny::observeEvent(
      input$file,
      {
        path <- input$file
        if (is.null(path) || !nzchar(path)) {
          return()
        }
        opened <- tryCatch(
          {
            doc <- st$proj$open(path)
            base <- am_text_content(navigate_to_text(doc$doc, st$at))
            list(doc = doc, base = base)
          },
          error = function(e) {
            shiny::showNotification(
              paste("Could not open file:", conditionMessage(e)),
              type = "error"
            )
            NULL
          }
        )
        if (is.null(opened)) {
          return()
        }
        st$doc <- opened$doc
        st$base <- opened$base
        st$shown <- opened$base
        rv$selected <- path
      },
      ignoreInit = TRUE
    )

    output$editor <- shiny::renderUI({
      sel <- rv$selected
      if (is.null(sel)) {
        return(editor_placeholder_ui())
      }
      editor_card_ui(sel, st$base, file_ext_dot(sel), debounce)
    })

    # Bidirectional editor <-> document sync, shared with the $edit() gadget;
    # reads the open document and tracking state from `st`.
    install_editor_sync(input, st)

    # --- Browse screen: refresh the file tree (picks up added/removed files) ---

    shiny::observeEvent(input$refresh, {
      if (is.null(st$proj)) {
        return()
      }
      st$proj$refresh()
      # Re-render the tree; output$filetree restores the active row from the
      # current selection. Drop the selection if its file is gone.
      paths <- st$proj$paths()
      if (!is.null(rv$selected) && !(rv$selected %in% paths)) {
        rv$selected <- NULL
      }
      rv$paths <- paths
    })

    # --- Browse screen: disconnect and return to the connect form ---

    shiny::observeEvent(input$disconnect, {
      cleanup_project(st)
      rv$selected <- NULL
      rv$view <- "connect"
    })

    # --- Connect screen: exit and end the app ---

    # Show a brief closing message (the "Session ended" screen), then stop the
    # gadget. The short delay lets the message render and unblocks the calling
    # R session.
    shiny::observeEvent(input$exit, {
      cleanup_project(st)
      rv$view <- "closed"
      later(function() shiny::stopApp(), delay = 0.75)
    })

    # Closing the window ends the session; disconnect so we never leak a socket.
    session$onSessionEnded(function() cleanup_project(st))
  }

  shiny::shinyApp(ui, app_server)
}

#' Close a project connection held in app state, if any
#'
#' @param st The app's state environment.
#'
#' @return Invisibly `NULL`.
#'
#' @noRd
cleanup_project <- function(st) {
  if (!is.null(st$proj)) {
    try(st$proj$close(), silent = TRUE)
    st$proj <- NULL
  }
  st$doc <- NULL
  invisible()
}

#' Build the connect-screen form card
#'
#' @param server,proj_id Initial values for the server URL and project ID
#'   fields.
#'
#' @return A bslib card.
#'
#' @noRd
connect_screen_ui <- function(server, proj_id) {
  bslib::card(
    style = "max-width: 520px; width: 100%;",
    bslib::card_header("Connect to a project"),
    bslib::card_body(
      shiny::textInput(
        "url",
        "Server URL",
        value = server,
        placeholder = "Sync server wss://",
        width = "100%"
      ),
      shiny::textInput(
        "proj_id",
        "Project ID",
        value = proj_id,
        placeholder = "Base58 document ID",
        width = "100%"
      ),
      shiny::div(
        class = "amsync-auth",
        shiny::div(
          class = "d-flex align-items-center gap-2",
          shiny::actionButton(
            "authenticate",
            "Authenticate",
            class = "btn-outline-primary"
          ),
          shiny::uiOutput("auth_status", inline = TRUE)
        ),
        # The OIDC fields are options of the Authenticate button, so present
        # them as a small disclosure nested beneath it rather than a full-width
        # accordion.
        shiny::tags$details(
          class = "amsync-advanced",
          shiny::tags$summary("Advanced"),
          shiny::div(
            class = "amsync-advanced-body",
            shiny::textInput(
              "client_id",
              "OIDC client ID",
              value = Sys.getenv("OIDC_CLIENT_ID"),
              width = "100%"
            ),
            shiny::passwordInput(
              "client_secret",
              "OIDC client secret",
              value = Sys.getenv("OIDC_CLIENT_SECRET"),
              width = "100%"
            ),
            shiny::textInput(
              "issuer",
              "OIDC issuer",
              value = oidc_issuer(),
              width = "100%"
            )
          )
        ),
        advanced_styles()
      )
    ),
    bslib::card_footer(
      shiny::div(
        class = "d-flex gap-2",
        shiny::actionButton(
          "connect",
          "Connect",
          class = "btn-primary flex-fill"
        ),
        shiny::actionButton(
          "exit",
          "Exit",
          class = "btn-outline-secondary"
        )
      )
    )
  )
}

#' CSS for the compact "Advanced" auth disclosure
#'
#' Renders the OIDC options as a small disclosure nested under the Authenticate
#' button: a subtle summary, a left border to show grouping, and smaller fields.
#'
#' @return A shiny `<style>` tag.
#'
#' @noRd
advanced_styles <- function() {
  shiny::tags$style(shiny::HTML(
    ".amsync-advanced { margin-top: 0.3rem; }
.amsync-advanced > summary {
  cursor: pointer; width: fit-content;
  font-size: 0.8rem; color: var(--bs-secondary-color);
}
.amsync-advanced-body {
  margin-top: 0.5rem; padding-left: 0.6rem;
  border-left: 2px solid var(--bs-border-color);
}
.amsync-advanced-body .form-label {
  font-size: 0.78rem; margin-bottom: 0.1rem;
}
.amsync-advanced-body .form-control {
  font-size: 0.8rem; padding: 0.2rem 0.5rem;
}
.amsync-advanced-body .shiny-input-container { margin-bottom: 0.5rem; }
.amsync-advanced-body .shiny-input-container:last-child { margin-bottom: 0; }"
  ))
}

#' Build the browse-screen sidebar layout
#'
#' The sidebar holds a collapsible file tree (rendered into `output$filetree`)
#' plus refresh/disconnect actions; the main area holds the editor output.
#'
#' @return A bslib sidebar layout.
#'
#' @noRd
browse_screen_ui <- function() {
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Files",
      width = 300,
      open = "open",
      file_tree_styles(),
      shiny::div(
        id = "amsync-filetree",
        class = "amsync-tree flex-fill overflow-auto",
        shiny::uiOutput("filetree")
      ),
      shiny::div(
        class = "d-flex gap-2 mt-auto",
        shiny::actionButton(
          "refresh",
          "Refresh",
          class = "btn-sm btn-outline-secondary flex-fill"
        ),
        shiny::actionButton(
          "disconnect",
          "Disconnect",
          class = "btn-sm btn-outline-danger flex-fill"
        )
      ),
      # Delegated click handler: a click on any file row sets input$file to the
      # row's path and moves the active highlight. Attached to the tree
      # container (recreated with this screen), so it never accumulates.
      shiny::tags$script(shiny::HTML(file_tree_js()))
    ),
    shiny::uiOutput("editor", fill = TRUE)
  )
}

#' Build the collapsible file tree from a set of paths
#'
#' Renders the project's file paths as nested `<details>` folders and clickable
#' file rows. Each file row stores its **original** path in a `data-path`
#' attribute (rather than one reconstructed from the displayed parts) so it
#' round-trips exactly to [amsync_project()]'s `$open()`.
#'
#' @param paths Character vector of file paths.
#' @param selected The currently-open path, highlighted as active, or `NULL`.
#'
#' @return A shiny tag list, or a muted placeholder when there are no files.
#'
#' @noRd
build_file_tree_ui <- function(paths, selected = NULL) {
  if (!length(paths)) {
    return(shiny::div(class = "text-muted small p-2", "No files in project."))
  }
  render_tree_nodes(path_tree(paths), selected)
}

#' Assemble paths into a nested tree keyed by path component
#'
#' Folders are named sub-lists; files are leaves carrying their original path
#' under `.path`. A leading slash (an empty first component) is dropped, so
#' `"/a/b.md"` and `"a/b.md"` nest identically while each leaf keeps its own
#' original key.
#'
#' @param paths Character vector of file paths.
#'
#' @return A nested named list.
#'
#' @noRd
path_tree <- function(paths) {
  tree <- list()
  for (p in paths) {
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    if (!length(parts)) {
      next
    }
    tree <- tree_insert_path(tree, parts, p)
  }
  tree
}

#' Insert one path's components into the nested tree
#'
#' @param node The (sub)tree to insert into.
#' @param parts Remaining path components.
#' @param path The file's original full path (stored at the leaf).
#'
#' @return The updated node.
#'
#' @noRd
tree_insert_path <- function(node, parts, path) {
  head <- parts[[1]]
  if (length(parts) == 1L) {
    node[[head]] <- list(.path = path)
    return(node)
  }
  child <- node[[head]]
  if (is.null(child) || !is.null(child$.path)) {
    child <- list()
  }
  node[[head]] <- tree_insert_path(child, parts[-1], path)
  node
}

#' Render one level of the file tree (directories first, then files)
#'
#' @param node A nested tree node (named list of children).
#' @param selected The active path, or `NULL`.
#'
#' @return A shiny tag list of folders and file rows.
#'
#' @noRd
render_tree_nodes <- function(node, selected) {
  keys <- names(node)
  is_file <- vapply(keys, function(k) !is.null(node[[k]]$.path), logical(1))
  ord <- order(is_file, tolower(keys))
  keys <- keys[ord]
  is_file <- is_file[ord]

  shiny::tagList(lapply(seq_along(keys), function(i) {
    key <- keys[i]
    child <- node[[key]]
    if (is_file[i]) {
      file_row_ui(key, child$.path, identical(child$.path, selected))
    } else {
      shiny::tags$details(
        open = NA,
        class = "amsync-folder",
        shiny::tags$summary(key),
        shiny::div(
          class = "amsync-folder-body",
          render_tree_nodes(child, selected)
        )
      )
    }
  }))
}

#' Build a single clickable file row
#'
#' @param label The file name shown.
#' @param path The file's original full path (stored in `data-path`).
#' @param active Whether this row is the open file.
#'
#' @return A shiny div.
#'
#' @noRd
file_row_ui <- function(label, path, active = FALSE) {
  classes <- c("amsync-file", if (isTRUE(active)) "active")
  shiny::tags$div(
    class = paste(classes, collapse = " "),
    `data-path` = path,
    role = "button",
    tabindex = "0",
    title = path,
    label
  )
}

#' CSS for the file tree (folders, file rows, hover and active states)
#'
#' @return A shiny `<style>` tag.
#'
#' @noRd
file_tree_styles <- function() {
  shiny::tags$style(shiny::HTML(
    ".amsync-tree { font-size: 0.875rem; }
.amsync-tree summary {
  cursor: pointer; padding: 2px 4px; border-radius: 4px;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
.amsync-tree summary:hover { background: var(--bs-secondary-bg); }
.amsync-folder-body {
  margin-left: 0.6rem; padding-left: 0.4rem;
  border-left: 1px solid var(--bs-border-color);
}
.amsync-file {
  cursor: pointer; padding: 2px 6px; border-radius: 4px;
  white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
}
.amsync-file:hover { background: var(--bs-secondary-bg); }
.amsync-file.active { background: var(--bs-primary); color: #fff; }
.amsync-file.active:hover { background: var(--bs-primary); }"
  ))
}

#' JavaScript that wires file-row clicks to `input$file`
#'
#' Uses event delegation on the tree container so it covers rows inserted by
#' later re-renders, and re-fires on re-click (priority "event") so re-opening
#' the same file works.
#'
#' @return A character scalar of JavaScript.
#'
#' @noRd
file_tree_js <- function() {
  "(function() {
  var root = document.getElementById('amsync-filetree');
  if (!root) { return; }
  root.addEventListener('click', function(e) {
    var el = e.target.closest('.amsync-file');
    if (!el || !root.contains(el)) { return; }
    root.querySelectorAll('.amsync-file.active').forEach(function(n) {
      n.classList.remove('active');
    });
    el.classList.add('active');
    Shiny.setInputValue('file', el.getAttribute('data-path'), {priority: 'event'});
  });
})();"
}

#' Build the live-editor card for an open file
#'
#' @param path The open file's path (shown in the header).
#' @param base The file's current content.
#' @param ext File extension (dotted) for syntax highlighting.
#' @param debounce Milliseconds to debounce outgoing editor changes.
#'
#' @return A bslib card containing a code editor and its streaming shim.
#'
#' @noRd
editor_card_ui <- function(path, base, ext, debounce) {
  bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between align-items-center",
      shiny::span(path),
      shiny::span(class = "text-muted small", "live")
    ),
    editor_body_ui(base, ext, debounce)
  )
}

#' Build the editor placeholder shown before a file is selected
#'
#' @return A shiny div.
#'
#' @noRd
editor_placeholder_ui <- function() {
  shiny::div(
    class = paste(
      "d-flex h-100 w-100 justify-content-center",
      "align-items-center text-muted p-4"
    ),
    "Select a file from the sidebar to edit."
  )
}

#' Build the closing screen shown after Exit
#'
#' @return A shiny div centred in the window.
#'
#' @noRd
closed_screen_ui <- function() {
  shiny::div(
    class = paste(
      "d-flex h-100 w-100 flex-column justify-content-center",
      "align-items-center text-center p-4"
    ),
    shiny::tags$h5(class = "mb-1", "Session ended"),
    shiny::tags$p(class = "text-muted mb-0", "You can close this window.")
  )
}

#' Build the sign-in status indicator for the connect form
#'
#' @param authed Whether a token has been obtained.
#'
#' @return A shiny span.
#'
#' @noRd
auth_status_ui <- function(authed) {
  if (isTRUE(authed)) {
    shiny::span(class = "text-success small", "\u2713 signed in")
  } else {
    shiny::span(class = "text-muted small", "not signed in")
  }
}
