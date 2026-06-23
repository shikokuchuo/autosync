# autosync

<!-- badges: start -->

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) [![R-CMD-check](https://github.com/shikokuchuo/autosync/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shikokuchuo/autosync/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/shikokuchuo/autosync/graph/badge.svg)](https://app.codecov.io/gh/shikokuchuo/autosync)
<!-- badges: end -->

R sync server and client for [Automerge](https://automerge.org/) CRDT documents. Implements the `automerge-repo` WebSocket protocol, enabling R to serve as a synchronization hub for Automerge clients in R, JavaScript, Rust, and other languages, and to act as a client that fetches, edits, and synchronizes documents hosted on remote servers.

[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/shikokuchuo/autosync)

### Features

- Full `automerge-repo` protocol compatibility (interoperates with sync.automerge.org)
- Secure connections via TLS (wss://)
- Persistent document storage
- Ephemeral message broadcasting
- Client functions for fetching documents from remote servers

## Installation

``` r
pak::pak("shikokuchuo/autosync")
```

## Server

Create a WebSocket sync server:

``` r
library(autosync)

server <- amsync_server()
server$start()
server

# Server runs non-blocking in the background

server$close()
```

With TLS for secure connections:

``` r
cert <- nanonext::write_cert()
tls <- nanonext::tls_config(server = cert$server)

server <- amsync_server(tls = tls)
server$start()
server$url
```

### Authentication

Enable OIDC authentication to restrict access. The defaults point to
Google, but any OIDC-compliant provider (Microsoft Entra, Okta, Auth0,
etc.) can be used by setting the `issuer` argument.

Register your OAuth client as a **"Desktop app"** and set the
`OIDC_CLIENT_ID` and `OIDC_CLIENT_SECRET` environment variables (e.g. in
`.Renviron`):

``` r
# .Renviron
OIDC_CLIENT_ID=YOUR_CLIENT_ID
OIDC_CLIENT_SECRET=YOUR_CLIENT_SECRET
```

Google issues a client secret for Desktop app clients and requires it in
the token exchange, but — unlike a "Web application" secret — it is not
treated as confidential: it is designed to be distributed with installed
apps, so it is safe to keep in `.Renviron`. Google states that for
installed apps "the client secret is obviously not treated as a secret"
([Google OAuth 2.0 docs][oauth-google]), consistent with the OAuth 2.0
for Native Apps standard ([RFC 8252 §8.5][rfc8252-85]). The interactive
flow also uses PKCE, so the secret alone does not grant access.

[oauth-google]: https://developers.google.com/identity/protocols/oauth2#installed
[rfc8252-85]: https://datatracker.ietf.org/doc/html/rfc8252#section-8.5

Other providers (Microsoft Entra, Okta, Auth0, etc.) can register the
client as a **native / public client**, which authenticates via PKCE
alone — for those, set only `OIDC_CLIENT_ID` and leave
`OIDC_CLIENT_SECRET` unset.

``` r
cert <- nanonext::write_cert()
tls <- nanonext::tls_config(server = cert$server)

server <- amsync_server(
  tls = tls,  # TLS required when auth is enabled
  auth = auth_config(allowed_domains = "posit.co")
)
server$start()
```

Clients obtain a token interactively, then pass it when fetching:

``` r
token <- amsync_token()

tlsclient <- nanonext::tls_config(client = cert$client)
doc <- amsync_fetch(
  server$url,
  "35ei6ouA7nLhtjmf3d9xk1KKvtKv",
  token = token,
  tls = tlsclient
)
```

### Sharing policy

The `share` parameter controls which documents are announced to clients and which requests are allowed. It accepts `TRUE`, `FALSE`, `NA`, or a function returning one of these per client and document:

| Value | Announce | Allow requests |
|-------|----------|----------------|
| `NA` (default) | No | Yes |
| `TRUE` | Yes | Yes |
| `FALSE` | No | No |

Announce all documents to every client that connects:

``` r
server <- amsync_server(share = TRUE)
```

Combine with authentication and use a `share` function for fine-grained access control. The function receives the `client_id` and `doc_id`, and can look up the authenticated email on the connection to decide:

``` r
# Allow list of emails that can access documents
allowed <- c("alice@example.com", "bob@example.com")

server <- amsync_server(
  tls = tls,
  auth = auth_config(),
  share = function(client_id, doc_id) {
    state <- attr(server, "sync")
    conn <- state$connections[[client_id]]
    if (conn$authenticated_email %in% allowed) NA else FALSE
  }
)
```

### Document management

``` r
# Create a new document
doc_id <- create_document(server)

# List all documents
list_documents(server)

# Get a document
doc <- get_document(server, doc_id)
```

### Connecting from JavaScript

``` javascript
import { Repo } from "@automerge/automerge-repo"
import { BrowserWebSocketClientAdapter } from "@automerge/automerge-repo-network-websocket"

const repo = new Repo({
  network: [new BrowserWebSocketClientAdapter("ws://localhost:8080")]
})
```

## Client

Fetch documents from any automerge-repo sync server:

``` r
# Fetch from public sync server
doc <- amsync_fetch("wss://sync.automerge.org", "your-document-id")

# Inspect document structure
str(doc)

# Or verbose fetch for debugging sync issues
doc <- amsync_fetch("wss://sync.automerge.org", "your-document-id", verbose = TRUE)
```

## Utilities

Generate document IDs compatible with automerge-repo:

``` r
doc_id <- generate_document_id()
```

## Links

- [automerge](https://posit-dev.github.io/automerge-r/) - R bindings for Automerge CRDT
