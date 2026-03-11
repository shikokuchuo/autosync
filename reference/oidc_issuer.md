# Default OIDC issuer URL

Returns the `OIDC_ISSUER` environment variable if set and non-empty,
otherwise falls back to Google (`"https://accounts.google.com"`).

## Usage

``` r
oidc_issuer()
```

## Value

Character string, the OIDC issuer URL.
