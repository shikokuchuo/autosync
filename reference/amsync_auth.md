# Get an access token for autosync authentication

Convenience wrapper around
[`gargle::token_fetch()`](https://gargle.r-lib.org/reference/token_fetch.html)
that obtains a token suitable for authenticating with an autosync
server.

## Usage

``` r
amsync_auth(
  email = gargle::gargle_oauth_email(),
  scopes = "https://www.googleapis.com/auth/userinfo.email",
  ...
)
```

## Arguments

- email:

  Email address to use for authentication, or NULL for interactive
  selection. See
  [`gargle::gargle_oauth_email()`](https://gargle.r-lib.org/reference/gargle_options.html).

- scopes:

  OAuth2 scopes. Defaults to userinfo.email which is sufficient for
  identity verification.

- ...:

  Additional arguments passed to
  [`gargle::token_fetch()`](https://gargle.r-lib.org/reference/token_fetch.html).

## Value

Character string containing the access token.

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive authentication
token <- amsync_auth()

# Use with amsync_fetch
doc <- amsync_fetch(
  url = "wss://secure-server.example.com",
  doc_id = "myDocId",
  access_token = token
)

# Specify email for non-interactive use
token <- amsync_auth(email = "user@example.com")
} # }
```
