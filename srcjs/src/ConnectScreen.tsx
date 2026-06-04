// Connect screen: server URL + project ID, an optional OIDC sign-in flow (run in
// R by amsync_token()), and Connect / Exit. Mirrors the former bslib connect card.
// The R server prefills the fields via `output$init`; events fire R observers.

import { React, useShinyEvent, useShinyInput, useShinyOutputValue } from "./shiny";

interface Init {
  server: string;
  proj_id: string;
  client_id: string;
  client_secret: string;
  issuer: string;
}

export function ConnectScreen() {
  const init = useShinyOutputValue<Init>("init");
  const authed = useShinyOutputValue<boolean>("authed", false) ?? false;

  const [url, setUrl] = useShinyInput<string>("url", "");
  const [projId, setProjId] = useShinyInput<string>("proj_id", "");
  const [clientId, setClientId] = useShinyInput<string>("client_id", "");
  const [clientSecret, setClientSecret] = useShinyInput<string>("client_secret", "");
  const [issuer, setIssuer] = useShinyInput<string>("issuer", "");

  const connect = useShinyEvent("connect");
  const exit = useShinyEvent("exit");

  // Seed the form once from the R-supplied prefill/defaults.
  const seeded = React.useRef(false);
  React.useEffect(() => {
    if (!init || seeded.current) return;
    seeded.current = true;
    if (init.server) setUrl(init.server);
    if (init.proj_id) setProjId(init.proj_id);
    if (init.client_id) setClientId(init.client_id);
    if (init.client_secret) setClientSecret(init.client_secret);
    if (init.issuer) setIssuer(init.issuer);
  }, [init, setUrl, setProjId, setClientId, setClientSecret, setIssuer]);

  return (
    <div className="amsync-connect">
      <div className="amsync-signin">
        <div className="amsync-brand">
          autosync<span className="amsync-brand-dot">.</span>
        </div>
        <p className="amsync-signin-subtitle">Connect to a sync project</p>
        {authed && <p className="amsync-signin-authed">✓ signed in</p>}

        <label className="amsync-field">
          <span>Server URL</span>
          <input
            type="text"
            value={url}
            placeholder="wss://…"
            onChange={(e) => setUrl(e.target.value)}
          />
        </label>
        <label className="amsync-field">
          <span>Project ID</span>
          <input
            type="text"
            value={projId}
            placeholder="Base58 document ID"
            onChange={(e) => setProjId(e.target.value)}
          />
        </label>

        <details className="amsync-advanced">
          <summary>Advanced</summary>
          <div className="amsync-advanced-body">
            <p className="amsync-advanced-note">
              Provide a client ID to sign in (OIDC) automatically when you
              connect. Leave blank for open servers.
            </p>
            <label className="amsync-field">
              <span>OIDC client ID</span>
              <input
                type="text"
                value={clientId}
                onChange={(e) => setClientId(e.target.value)}
              />
            </label>
            <label className="amsync-field">
              <span>OIDC client secret</span>
              <input
                type="password"
                value={clientSecret}
                onChange={(e) => setClientSecret(e.target.value)}
              />
            </label>
            <label className="amsync-field">
              <span>OIDC issuer</span>
              <input
                type="text"
                value={issuer}
                onChange={(e) => setIssuer(e.target.value)}
              />
            </label>
          </div>
        </details>

        <button type="button" className="amsync-primary-btn" onClick={connect}>
          Connect
        </button>
        <button type="button" className="amsync-ghost-btn" onClick={exit}>
          Exit
        </button>
      </div>
    </div>
  );
}
