# Authenticate a client from peerMetadata

Extracts and validates the access token from a client's peerMetadata.

## Usage

``` r
authenticate_client(auth_config, peer_metadata)
```

## Arguments

- auth_config:

  An amsync_auth_config object.

- peer_metadata:

  List containing client's peer metadata.

## Value

List with `valid` (logical), `email` (character or NULL), `error`
(character or NULL).
