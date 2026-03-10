# Validate a JWT token

Verifies a JWT locally using the issuer's public keys (JWKS) and
validates standard OIDC claims.

## Usage

``` r
validate_token(
  token,
  issuer,
  client_id,
  allowed_emails = NULL,
  allowed_domains = NULL,
  custom_validator = NULL
)
```

## Arguments

- token:

  Character string, the JWT.

- issuer:

  Expected issuer URL.

- client_id:

  Expected audience (client ID).

- allowed_emails:

  Character vector of allowed email addresses (optional).

- allowed_domains:

  Character vector of allowed email domains (optional).

- custom_validator:

  Function(claims) returning TRUE/FALSE (optional).

## Value

List with `valid` (logical), `email` (character or NULL), `error`
(character or NULL).
