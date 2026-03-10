# Fetch and parse a JWKS endpoint

Fetch and parse a JWKS endpoint

## Usage

``` r
fetch_jwks(jwks_uri)
```

## Arguments

- jwks_uri:

  URL of the JWKS endpoint.

## Value

List with `keys` (named list of jose key objects by kid) and `expiry`
(POSIXct when the cache should be refreshed).
