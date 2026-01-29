# Check if a pending connection has timed out

Called by later::later() to check if a client authenticated in time.

## Usage

``` r
check_auth_timeout(server, temp_id)
```

## Arguments

- server:

  Server state environment.

- temp_id:

  Temporary connection ID.
