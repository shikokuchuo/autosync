# Obtain an OIDC token interactively

Performs the OAuth 2.0 Authorization Code flow with PKCE to obtain a JWT
(ID token) from an OIDC provider, delegating the browser handshake and
token exchange to httr2. Endpoints are discovered from the issuer's
`.well-known` metadata via
[`httr2::oauth_server_metadata()`](https://httr2.r-lib.org/reference/oauth_server_metadata.html),
and the flow is run by
[`httr2::oauth_flow_auth_code()`](https://httr2.r-lib.org/reference/req_oauth_auth_code.html):
it opens the system browser, listens on a loopback redirect for the
callback, and returns the ID token for use with
[`amsync_fetch()`](http://shikokuchuo.net/autosync/reference/amsync_fetch.md).

## Usage

``` r
amsync_token(
  client_id = Sys.getenv("OIDC_CLIENT_ID"),
  client_secret = Sys.getenv("OIDC_CLIENT_SECRET"),
  issuer = oidc_issuer(),
  scopes = "openid email",
  redirect_uri = oauth_redirect_uri()
)
```

## Arguments

- client_id:

  The OIDC client ID (application ID). Defaults to the `OIDC_CLIENT_ID`
  environment variable.

- client_secret:

  The OIDC client secret. Required by Google (Desktop app) and "Web
  application" client types; leave unset for native / public clients,
  which authenticate via PKCE alone. Defaults to the
  `OIDC_CLIENT_SECRET` environment variable.

- issuer:

  The OIDC issuer URL. Defaults to the `OIDC_ISSUER` environment
  variable, falling back to Google (`"https://accounts.google.com"`).

- scopes:

  Space-separated OAuth scopes to request. Default `"openid email"`.

- redirect_uri:

  Local redirect URI for the OAuth callback. Defaults to
  [`httr2::oauth_redirect_uri()`](https://httr2.r-lib.org/reference/oauth_redirect_uri.html),
  i.e. `"http://localhost"` with an OS-assigned random port (or the
  `HTTR2_OAUTH_REDIRECT_URL` environment variable on hosted platforms).
  Supply an explicit port (e.g. `"http://localhost:8080"`) when your
  OIDC provider requires the redirect URI to match a pre-registered
  value.

## Value

A JWT (ID token) as a character string.

## Details

For Google, register the OAuth client as a "Desktop app" and set both
`OIDC_CLIENT_ID` and `OIDC_CLIENT_SECRET`. Google's Desktop app secret
is required in the token exchange but, unlike a "Web application"
secret, is not treated as confidential: Google states that for installed
apps "the client secret is obviously not treated as a secret"
(<https://developers.google.com/identity/protocols/oauth2#installed>),
consistent with the OAuth 2.0 for Native Apps standard (RFC 8252 section
8.5, <https://datatracker.ietf.org/doc/html/rfc8252#section-8.5>).
Providers that support native / public clients (Microsoft Entra, Okta,
Auth0, etc.) need only `client_id`, authenticating via PKCE alone; leave
`client_secret` unset for these.

## Examples

``` r
if (FALSE) { # interactive()
# Uses OIDC_CLIENT_ID and OIDC_CLIENT_SECRET env vars by default
token <- amsync_token()

# Or supply credentials directly
token <- amsync_token(
  client_id = "YOUR_CLIENT_ID.apps.googleusercontent.com",
  client_secret = "YOUR_CLIENT_SECRET"
)

# Use with amsync_fetch
doc <- amsync_fetch(server$url, "myDocId", token = token, tls = tls)
}
```
