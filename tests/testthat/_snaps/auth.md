# auth_config validates timeout parameters

    Code
      auth_config(auth_timeout = -1)
    Condition
      Error in `auth_config()`:
      ! 'auth_timeout' must be a positive number

---

    Code
      auth_config(auth_timeout = "10")
    Condition
      Error in `auth_config()`:
      ! 'auth_timeout' must be a positive number

---

    Code
      auth_config(token_timeout = 0)
    Condition
      Error in `auth_config()`:
      ! 'token_timeout' must be a positive number

---

    Code
      auth_config(token_timeout = c(1, 2))
    Condition
      Error in `auth_config()`:
      ! 'token_timeout' must be a positive number

# server requires TLS when auth is enabled

    Code
      amsync_server(port = get_test_port(), auth = auth_config(allowed_emails = "test@example.com"))
    Condition
      Error in `amsync_server()`:
      ! Authentication requires TLS. Provide a 'tls' configuration.
      Transmitting OAuth tokens over unencrypted connections is a security risk.

# server rejects unauthenticated client when auth enabled

    Code
      amsync_fetch(url = sprintf("wss://127.0.0.1:%d", port), doc_id = generate_document_id(),
      tls = client_tls)
    Condition
      Error in `amsync_fetch()`:
      ! Server error: Authentication failed

