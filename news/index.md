# Changelog

## autosync (development version)

- [`amsync_client()`](http://shikokuchuo.net/autosync/reference/amsync_client.md)
  now opens a connection only and no longer takes a `doc_id`. Open one
  or more live documents over the connection with the new
  `$open_doc(doc_id)` method, which returns an `amsync_doc` handle
  exposing `$doc`, `$push()`, `$active` and `$close()`. A single
  connection can sync several documents, and `$close()` tears them all
  down.
- [`amsync_project()`](http://shikokuchuo.net/autosync/reference/amsync_project.md)
  keeps a persistent connection to the server: the project document and
  every file you `$open()`, `$edit()` or `$browse()` sync over the same
  WebSocket rather than dialing the server again. Call the new
  `$close()` when finished.
- `amsync_edit()` now operates on an `amsync_doc` handle (from
  `amsync_client()$open_doc()`).
- [`amsync_project()`](http://shikokuchuo.net/autosync/reference/amsync_project.md)
  browses a project document’s file tree from just a server URL and
  project ID, and edits files by path. `$browse()` and `$edit()` pick a
  file from a Shiny app, then hand off to `amsync_edit()`.
- `amsync_edit()` opens a synced text object in a live Shiny editor (a
  [`bslib::input_code_editor()`](https://rstudio.github.io/bslib/reference/input_code_editor.html)
  component): edits stream into the live document as you type (debounced
  via the `debounce` argument) and remote changes update the editor
  automatically. There is no **Save** button. Requires the `shiny` and
  `bslib` packages.

## autosync 0.0.1

- Initial version.
