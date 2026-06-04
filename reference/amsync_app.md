# Launch the autosync browser app

Opens a single Shiny app that carries the whole workflow from start to
finish without any other R commands. It has two screens served in one
window:

## Usage

``` r
amsync_app(
  server = "",
  proj_id = "",
  token = NULL,
  tls = NULL,
  timeout = 5000L,
  files_key = "files",
  debounce = 300L
)
```

## Arguments

- server:

  Initial sync-server URL to prefill in the connect form. Default `""`.

- proj_id:

  Initial project document ID to prefill. Default `""`.

- token:

  (optional) A JWT obtained earlier from
  [`amsync_token()`](http://shikokuchuo.net/autosync/reference/amsync_token.md).
  When supplied, the app starts already signed in; you can still
  re-authenticate from the form. Default `NULL` (sign in from the form,
  or connect with no token).

- tls:

  (optional) for secure wss:// connections to servers with self-signed
  or custom CA certificates, a TLS configuration object created by
  [`nanonext::tls_config()`](https://nanonext.r-lib.org/reference/tls_config.html).

- timeout:

  Timeout in milliseconds for each receive operation. Default 5000.

- files_key:

  Key of the files map within the project document. Default `"files"`.

- debounce:

  Milliseconds to debounce outgoing editor changes, passed through to
  the live editor. Default 300.

## Value

Invisibly `NULL`, when the app window is closed.

## Details

- **Connect** – enter a sync-server URL and a project document ID, and
  optionally authenticate. The **Authenticate** button runs the same
  OIDC browser flow as
  [`amsync_token()`](http://shikokuchuo.net/autosync/reference/amsync_token.md);
  client ID, secret, and issuer can be set under **Advanced** (prefilled
  from the `OIDC_CLIENT_ID`, `OIDC_CLIENT_SECRET`, and `OIDC_ISSUER`
  environment variables). Passing a `token` obtained earlier from
  [`amsync_token()`](http://shikokuchuo.net/autosync/reference/amsync_token.md)
  starts the app already signed in, skipping that step. Leaving the
  sign-in untouched connects without a token, for open servers.

- **Browse & edit** – once connected, the project's file tree appears in
  a sidebar; selecting a file opens its document in a live
  [`bslib::input_code_editor()`](https://rstudio.github.io/bslib/reference/input_code_editor.html)
  that stays in sync with the server in both directions, just like a
  document handle's `$edit()` method. **Disconnect** returns to the
  connect screen; closing the window ends the session.

This is a front door to
[`amsync_project()`](http://shikokuchuo.net/autosync/reference/amsync_project.md):
it builds the same connection and reuses it for every file opened during
the session.

Requires the shiny and bslib packages and an interactive session.

## Examples

``` r
if (FALSE) { # interactive()
# Start with empty fields and fill them in the form:
amsync_app()

# Or prefill the server and project so only sign-in/Connect remain:
amsync_app("wss://quarto-hub.com/ws", proj_id = "4F63WJPDzbHkkfKa66h1Qrr1sC5U")

# Reuse a token obtained earlier, so the app starts signed in:
token <- amsync_token()
amsync_app("wss://quarto-hub.com/ws", proj_id = "4F63WJPD...", token = token)
}
```
