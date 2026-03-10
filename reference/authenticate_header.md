# Authenticate a client from HTTP request headers

Extracts and validates a Bearer token (JWT) from the Authorization
header of the WebSocket upgrade request.

## Usage

``` r
authenticate_header(auth_config, headers)
```

## Arguments

- auth_config:

  An amsync_auth_config object.

- headers:

  Named list of HTTP request headers.

## Value

List with `valid` (logical), `email` (character or NULL), `error`
(character or NULL).
