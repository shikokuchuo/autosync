# autosync (development version)

* The `amsync_*` connector functions were renamed `sync_*`: `sync_server()`,
  `sync_client()`, `sync_fetch()` and `sync_token()` (with S3 classes
  `sync_server`, `sync_client` and `sync_doc`).
* `sync_client()` now opens a connection only and no longer takes a `doc_id`.
  Open one or more live documents over the connection with the new
  `$open_doc(doc_id)` method, which returns a `sync_doc` handle exposing
  `$doc`, `$push()`, `$active` and `$close()`. A single connection can sync
  several documents, and `$close()` tears them all down.
* Project browsing and live editing moved to the `shinysync` package
  (`project_open()`, `project_app()` and `project_edit()`); autosync no longer
  depends on `shiny` or `bslib`.

# autosync 0.0.1

* Initial version.
