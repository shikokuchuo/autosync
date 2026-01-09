# Start the sync server (blocking)

Starts the WebSocket server and enters the event loop. This function
blocks until the server is stopped via
[`stop_server()`](http://shikokuchuo.net/autosync/reference/stop_server.md).

## Usage

``` r
serve(server)
```

## Arguments

- server:

  An amsync_server object.

## Value

Invisibly returns the server object.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- amsync_server()
serve(server)  # Blocks until stopped
} # }
```
