# Get a signing key for JWT verification

Looks up the public key for the given issuer and key ID, refreshing the
cache if needed.

## Usage

``` r
get_signing_key(issuer, kid)
```

## Arguments

- issuer:

  The OIDC issuer URL.

- kid:

  The key ID from the JWT header.

## Value

A public key object, or NULL if not found.
