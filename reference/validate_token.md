# Validate an OAuth2 access token

Validates an access token by calling Google's tokeninfo endpoint.

## Usage

``` r
validate_token(
  access_token,
  allowed_emails = NULL,
  allowed_domains = NULL,
  custom_validator = NULL,
  token_timeout = 5
)
```

## Arguments

- access_token:

  Character string, the OAuth2 access token.

- allowed_emails:

  Character vector of allowed email addresses (optional).

- allowed_domains:

  Character vector of allowed email domains (optional).

- custom_validator:

  Function(token_info) returning TRUE/FALSE (optional).

- token_timeout:

  Numeric, seconds to wait for Google tokeninfo endpoint (default 5).

## Value

List with `valid` (logical), `email` (character or NULL), `error`
(character or NULL).
