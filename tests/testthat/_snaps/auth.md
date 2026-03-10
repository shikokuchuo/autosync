# auth_config validates issuer and client_id

    Code
      auth_config(issuer = 123, client_id = "x")
    Condition
      Error in `auth_config()`:
      ! 'issuer' must be a single character string (OIDC issuer URL)

---

    Code
      auth_config(issuer = "x", client_id = 123)
    Condition
      Error in `auth_config()`:
      ! 'client_id' must be set (or set the OIDC_CLIENT_ID environment variable)

---

    Code
      auth_config(client_id = "")
    Condition
      Error in `auth_config()`:
      ! 'client_id' must be set (or set the OIDC_CLIENT_ID environment variable)

# server requires TLS when auth is enabled

    Code
      amsync_server(auth = auth_config(issuer = "https://accounts.google.com",
        client_id = "test-id"))
    Condition
      Error in `amsync_server()`:
      ! Authentication requires TLS. Provide a 'tls' configuration.
      Transmitting tokens over unencrypted connections is a security risk.

# server rejects unauthenticated client when auth enabled

    Code
      amsync_fetch(url = server$url, doc_id = generate_document_id(), tls = client_tls)
    Condition
      Error in `amsync_fetch()`:
      ! Server error: Authentication failed

