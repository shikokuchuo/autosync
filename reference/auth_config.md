# Create an authentication configuration

Creates a configuration object for enabling Google OAuth2 authentication
on an autosync server.

## Usage

``` r
auth_config(
  allowed_emails = NULL,
  allowed_domains = NULL,
  custom_validator = NULL,
  auth_timeout = 10,
  token_timeout = 5
)
```

## Arguments

- allowed_emails:

  Character vector of allowed email addresses.

- allowed_domains:

  Character vector of allowed email domains (e.g., "mycompany.com").

- custom_validator:

  Function(token_info) returning TRUE/FALSE for custom validation logic.

- auth_timeout:

  Numeric, seconds to wait for client to send join message with valid
  credentials after WebSocket connection is established. Connections
  that don't authenticate within this window are closed. Default 10
  seconds.

- token_timeout:

  Numeric, seconds to wait for Google's tokeninfo endpoint to respond
  when validating tokens. Default 5 seconds.

## Value

An amsync_auth_config object.

## Examples

``` r
# Allow specific email domains
auth_config(allowed_domains = c("mycompany.com", "partner.org"))
#> $allowed_emails
#> NULL
#> 
#> $allowed_domains
#> [1] "mycompany.com" "partner.org"  
#> 
#> $custom_validator
#> NULL
#> 
#> $auth_timeout
#> [1] 10
#> 
#> $token_timeout
#> [1] 5
#> 
#> attr(,"class")
#> [1] "amsync_auth_config"

# Allow specific users
auth_config(allowed_emails = c("alice@example.com", "bob@example.com"))
#> $allowed_emails
#> [1] "alice@example.com" "bob@example.com"  
#> 
#> $allowed_domains
#> NULL
#> 
#> $custom_validator
#> NULL
#> 
#> $auth_timeout
#> [1] 10
#> 
#> $token_timeout
#> [1] 5
#> 
#> attr(,"class")
#> [1] "amsync_auth_config"

# Custom validator
auth_config(custom_validator = function(token_info) {
  # Only allow tokens with at least 5 minutes remaining
  as.integer(token_info$expires_in) > 300
})
#> $allowed_emails
#> NULL
#> 
#> $allowed_domains
#> NULL
#> 
#> $custom_validator
#> function (token_info) 
#> {
#>     as.integer(token_info$expires_in) > 300
#> }
#> <environment: 0x55dd22205af8>
#> 
#> $auth_timeout
#> [1] 10
#> 
#> $token_timeout
#> [1] 5
#> 
#> attr(,"class")
#> [1] "amsync_auth_config"
```
