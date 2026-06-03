# autosync (development version)

* `amsync_client()` now opens a connection only and no longer takes a `doc_id`.
  Open one or more live documents over the connection with the new
  `$open_doc(doc_id)` method, which returns an `amsync_doc` handle exposing
  `$doc`, `$push()`, `$active` and `$close()`. A single connection can sync
  several documents, and `$close()` tears them all down.
* `amsync_project()` keeps a persistent connection to the server: the project
  document and every file you `$open()`, `$edit()` or `$browse()` sync over the
  same WebSocket rather than dialing the server again. Call the new `$close()`
  when finished.
* `amsync_edit()` now operates on an `amsync_doc` handle (from
  `amsync_client()$open_doc()`).
* `amsync_project()` browses a project document's file tree from just a server
  URL and project ID, and edits files by path. `$browse()` and `$edit()` pick a
  file from a Shiny app, then hand off to `amsync_edit()`.
* `amsync_edit()` opens a synced text object in a Shiny app with a
  `bslib::input_code_editor()` component, merging your edits back into the live
  document on **Save** while preserving concurrent remote edits. Requires the
  `shiny` and `bslib` packages.

# autosync 0.0.1

* Initial version.
