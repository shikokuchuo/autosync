# Create an authentication configuration

Creates a configuration object for enabling OIDC JWT authentication on
an autosync server. When enabled, clients must include a valid JWT (ID
token) as a Bearer token in the Authorization header of the WebSocket
upgrade request. Connections without valid credentials are rejected
immediately at connection time.

## Usage

``` r
auth_config(
  issuer = oidc_issuer(),
  client_id = Sys.getenv("OIDC_CLIENT_ID"),
  allowed_emails = NULL,
  allowed_domains = NULL,
  custom_validator = NULL
)
```

## Arguments

- issuer:

  The OIDC issuer URL. This is used to discover the provider's public
  keys via the `.well-known/openid-configuration` endpoint, and to
  validate the `iss` claim in JWTs. Defaults to the `OIDC_ISSUER`
  environment variable, falling back to Google
  (`"https://accounts.google.com"`).

- client_id:

  The OIDC client ID (application ID). Validated against the `aud` claim
  in JWTs. Defaults to the `OIDC_CLIENT_ID` environment variable.

- allowed_emails:

  Character vector of allowed email addresses.

- allowed_domains:

  Character vector of allowed email domains (e.g., "mycompany.com").

- custom_validator:

  Function(claims) returning TRUE/FALSE for custom validation logic.
  Receives the decoded JWT claims as a list.

## Value

An amsync_auth_config object.

## Details

Works with any OIDC-compliant identity provider: Google, Microsoft
Entra, Okta, Auth0, etc.

## Examples

``` r
# Google (default issuer)
auth_config(
  client_id = "123456789.apps.googleusercontent.com",
  allowed_domains = "mycompany.com"
)
#> $issuer
#> [1] "https://accounts.google.com"
#> 
#> $client_id
#> [1] "123456789.apps.googleusercontent.com"
#> 
#> $allowed_emails
#> NULL
#> 
#> $allowed_domains
#> [1] "mycompany.com"
#> 
#> $custom_validator
#> NULL
#> 
#> attr(,"class")
#> [1] "amsync_auth_config"

# Microsoft Entra
auth_config(
  issuer = "https://login.microsoftonline.com/common/v2.0",
  client_id = "abcdef-1234-5678",
  allowed_emails = "alice@mycompany.com"
)
#> $issuer
#> [1] "https://login.microsoftonline.com/common/v2.0"
#> 
#> $client_id
#> [1] "abcdef-1234-5678"
#> 
#> $allowed_emails
#> [1] "alice@mycompany.com"
#> 
#> $allowed_domains
#> NULL
#> 
#> $custom_validator
#> NULL
#> 
#> attr(,"class")
#> [1] "amsync_auth_config"

# Custom validator
auth_config(
  issuer = "https://dev-123456.okta.com/oauth2/default",
  client_id = "0oaXXXXXXXX",
  custom_validator = function(claims) "editors" %in% claims$groups
)
#> $issuer
#> [1] "https://dev-123456.okta.com/oauth2/default"
#> 
#> $client_id
#> [1] "0oaXXXXXXXX"
#> 
#> $allowed_emails
#> NULL
#> 
#> $allowed_domains
#> NULL
#> 
#> $custom_validator
#> function (claims) 
#> "editors" %in% claims$groups
#> <environment: 0x55ebf6ae3d40>
#> 
#> attr(,"class")
#> [1] "amsync_auth_config"
```
