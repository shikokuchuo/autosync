# RFC: Automerge-Repo Sync Protocol

**Status:** Informational
**Version:** 1.0
**Date:** 2026-03-07

## Abstract

This document specifies the wire protocol used by automerge-repo sync servers and clients to synchronize Automerge CRDT documents over WebSocket connections. The protocol consists of a CBOR-encoded message exchange over WebSocket binary frames, beginning with a handshake phase followed by a continuous sync phase. This specification is intended to enable interoperable implementations in any programming language.

## Table of Contents

1. [Introduction](#1-introduction)
2. [Terminology](#2-terminology)
3. [Conventions](#3-conventions)
4. [Transport Layer](#4-transport-layer)
5. [Message Encoding](#5-message-encoding)
6. [Identifiers](#6-identifiers)
7. [Handshake Phase](#7-handshake-phase)
8. [Sync Phase](#8-sync-phase)
9. [Message Types](#9-message-types)
10. [Connection Maintenance](#10-connection-maintenance)
11. [Sync Server Behavior](#11-sync-server-behavior)
12. [Remote Heads Gossiping](#12-remote-heads-gossiping)
13. [Security Considerations](#13-security-considerations)

---

## 1. Introduction

Automerge-repo is a framework for synchronizing Automerge CRDT documents across peers. The sync server acts as a persistent, always-on peer that stores documents and relays sync messages between clients. A single WebSocket connection multiplexes sync traffic for many documents simultaneously.

The protocol is transport-agnostic at its core (any reliable, ordered, bidirectional byte stream will work), but this RFC describes the WebSocket binding.

### 1.1 Design Goals

- **Simplicity:** Minimal handshake, then standard Automerge sync messages.
- **Multiplexing:** A single connection handles all documents.
- **Stateless wire format:** Each message is self-contained (includes sender, target, and document identifiers).
- **Compatibility:** Built on the Automerge sync protocol, which handles conflict-free merging.

---

## 2. Terminology

| Term | Definition |
|------|-----------|
| **Initiating peer** | The peer that opens the connection (WebSocket client). |
| **Receiving peer** | The peer that accepts the connection (WebSocket server). |
| **Peer ID** | An ephemeral string identifier for a peer, unique per process lifetime. |
| **Storage ID** | A persistent unique identifier tied to a peer's durable storage. SHOULD be generated from 16 random bytes (e.g., formatted as a UUID v4 or hex string), but MAY be any unique string. Multiple peer IDs may share the same storage ID. Optional. |
| **Document ID** | A base58check-encoded unique identifier for an Automerge document. SHOULD be 16 random bytes, but MAY be any unique byte sequence. |
| **Automerge URL** | A document reference of the form `automerge:<documentId>`. |
| **Sync message** | The binary output of Automerge's sync message generation -- an opaque blob that the Automerge library knows how to produce and consume. |
| **Ephemeral message** | A non-persisted application-level message (e.g., cursor positions, presence). |
| **Session ID** | A random string generated once per repo instance, used for ephemeral message deduplication. |
| **CBOR** | Concise Binary Object Representation ([RFC 8949](https://datatracker.ietf.org/doc/html/rfc8949)). |
| **CDDL** | Concise Data Definition Language ([RFC 8610](https://datatracker.ietf.org/doc/html/rfc8610)), used to describe message schemas in this RFC. |

---

## 3. Conventions

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://datatracker.ietf.org/doc/html/rfc2119).

---

## 4. Transport Layer

### 4.1 WebSocket Binding

The protocol operates over a single WebSocket connection per client-server pair. The WebSocket MUST use binary frames (not text frames).

**Connection establishment:**

1. The client opens a WebSocket connection to the server's URL.
2. The server handles HTTP upgrade requests to establish WebSocket connections.
3. Upon successful WebSocket connection, the handshake phase begins immediately.

### 4.2 Framing

Each WebSocket message is a single, complete CBOR-encoded object. There is no additional framing -- one WebSocket binary frame equals one protocol message.

**Constraint:** Implementations MUST NOT send zero-length messages. Implementations SHOULD reject received zero-length messages.

### 4.3 Applicability to Other Transports

Although specified here for WebSocket, the message format and protocol flow apply to any reliable, ordered, bidirectional byte stream (e.g., TCP, QUIC). When adapting to other transports, an appropriate length-prefix framing MUST be added since those transports do not have built-in message boundaries.

---

## 5. Message Encoding

### 5.1 CBOR Configuration

All messages are encoded using CBOR with the following **encoder** configuration:

- Byte-string values are encoded as CBOR byte strings (major type 2) without an additional tag.
- Objects are encoded as CBOR maps with string keys (not as CBOR arrays using record-style compression).

These settings affect only encoding. Decoding uses default CBOR decoding with no special configuration -- any standards-compliant CBOR library can decode messages without additional options.

### 5.2 Binary Data

Binary fields (e.g., `data` in sync messages) are encoded as CBOR byte strings (major type 2). When sending over WebSocket, the entire CBOR-encoded message is sent as a single binary frame.

### 5.3 Sending

When sending a CBOR-encoded message over WebSocket, implementations MUST ensure only the relevant byte range is transmitted. If the encoded buffer is a view on a larger allocation, the implementation MUST extract the exact byte range before sending.

---

## 6. Identifiers

### 6.1 Peer ID

A peer ID is an opaque, non-empty string that uniquely identifies a peer for the lifetime of its process. There are no format constraints beyond being a non-empty string.

Peer IDs are ephemeral -- they are not expected to survive process restarts.

### 6.2 Storage ID

A storage ID is an OPTIONAL persistent identifier tied to a peer's durable storage backend. It SHOULD be a string derived from 16 random bytes (e.g., formatted as a UUID v4 or a hex string), but MAY be any unique string. Implementations MUST treat storage IDs as opaque strings and MUST NOT assume any particular format. Storage IDs allow peers to:

- Persist and reload sync state across connection lifetimes
- Identify when multiple peer IDs share the same underlying storage

A peer that has no persistent storage SHOULD set `isEphemeral: true` in its metadata and MAY omit the `storageId`.

### 6.3 Document ID

Document IDs are unique byte sequences encoded using base58check. They SHOULD be 16 random bytes, but MAY be any unique byte sequence. Implementations MUST treat document IDs as opaque base58check-encoded strings and MUST NOT assume a particular decoded length or format. The base58check encoding provides a compact string representation with a built-in checksum.

**Recommended encoding procedure:**
1. Generate 16 cryptographically random bytes.
2. Encode the bytes using base58check.

**Automerge URL format:** `automerge:<base58check-documentId>`

Example: `automerge:4NMNnkMhL8jXrdJ9jamS58PAVdXu`

### 6.4 Session ID

A session ID is a random string generated once when a repo instance starts. It is used in ephemeral messages for deduplication (see [Section 9.8](#98-ephemeral)).

---

## 7. Handshake Phase

The handshake establishes peer identities and negotiates the protocol version. It MUST complete before any sync messages are exchanged.

### 7.1 Flow

```
Initiating Peer (Client)              Receiving Peer (Server)
        |                                       |
        |-------- join message ---------------->|
        |                                       |
        |    [validate protocol version]        |
        |                                       |
        |<------- peer message -----------------|
        |                                       |
        |    [both enter sync phase]            |
        |                                       |
```

### 7.2 Detailed Steps

1. **Client sends `join` message:** Immediately after the WebSocket connection opens, the initiating peer sends a `join` message containing:
   - Its peer ID (`senderId`)
   - Its peer metadata (storage ID, ephemeral flag)
   - Supported protocol versions (currently `["1"]`)

2. **Server waits for `join`:** The receiving peer MUST NOT send any messages before receiving a `join` message from the initiating peer. The receiving peer MUST wait for the first message and verify it is a `join`.

3. **Server validates and responds:**
   - If `supportedProtocolVersions` is present and does not contain any version supported by the server, the server MUST send an `error` message and close the connection.
   - If `supportedProtocolVersions` is absent, the server MUST assume protocol version `"1"` (for backward compatibility).
   - Otherwise, the server sends a `peer` message back containing its own peer ID, metadata, and `selectedProtocolVersion`.

4. **Client processes `peer` response:**
   - When the client receives a `peer` message, it stores the server's peer ID and metadata, and enters the sync phase.
   - If the client receives an `error` message, it SHOULD log the error and close the connection.

### 7.3 Reconnection

If a client reconnects (sends a new `join` with the same `senderId`), the server MUST:
1. Close the existing WebSocket for that peer ID (if still open).
2. Process the new `join` as a fresh connection.

### 7.4 Ready Timeout

Clients SHOULD implement a ready timeout. If no `peer` response is received within this timeout, the client SHOULD proceed as ready to avoid blocking indefinitely. A timeout of 1 second is RECOMMENDED.

---

## 8. Sync Phase

Once the handshake completes, both peers enter the sync phase. In this phase, either peer may send any of the following message types at any time:

- `request` -- Ask to begin sync for a document
- `sync` -- Send an Automerge sync message for a document
- `doc-unavailable` -- Indicate a document is not available
- `ephemeral` -- Send a non-persisted application message
- `remote-subscription-change` -- Modify remote heads subscriptions
- `remote-heads-changed` -- Notify of remote document state changes
- `leave` -- Advisory message before disconnect

### 8.1 Document Sync Flow

The Automerge sync protocol is a peer-to-peer algorithm where each side maintains a sync state per peer per document. The flow for a single document:

```
Client                                  Server
   |                                       |
   |--- request (documentId, syncMsg) ---->|
   |                                       |
   |    [server loads doc, generates       |
   |     response sync message]            |
   |                                       |
   |<-- sync (documentId, syncMsg) --------|
   |                                       |
   |--- sync (documentId, syncMsg) ------->|
   |                                       |
   |    ... (repeat until converged) ...   |
   |                                       |
```

**Key rules:**

1. The first message from a peer that does not yet have a document SHOULD be a `request` (not `sync`). This tells the recipient to respond with `doc-unavailable` if it does not have the document.
2. Subsequent messages for the same document use the `sync` type.
3. The Automerge sync protocol converges -- once both peers have the same document state, no more messages are sent for that document (until a change occurs).
4. A peer that receives a `request` for a document it does not have and cannot obtain from any of its peers SHOULD respond with `doc-unavailable`.

### 8.2 Share Policy

Implementations MAY use a **generous share policy** (proactively syncing all documents with all peers) or a **non-generous share policy** (only syncing documents that clients explicitly request by document ID).

Implementations SHOULD support the following policy controls:

- **Announce policy:** Whether to proactively send sync messages for a document to a peer without being asked. Sync servers SHOULD return `true` or `false`.
- **Access policy:** Whether a peer is allowed to sync a specific document. Can be used for access control. If the access policy denies a request, the server SHOULD respond with `doc-unavailable`.

---

## 9. Message Types

All messages are CBOR-encoded maps with string keys. Schemas are defined in [CDDL](https://datatracker.ietf.org/doc/html/rfc8610).

### 9.0 Common Types (CDDL Preamble)

```cddl
; An opaque string identifying a peer (ephemeral, per-process).
peer_id = str

; A persistent storage identifier (SHOULD be derived from 16 random bytes; any unique string is valid)
storage_id = str

; Protocol version identifier
protocol_version = "1"

; Raw bytes of an Automerge sync message
sync_message = bstr

; Base58check-encoded document identifier (SHOULD be 16 random bytes; any unique byte sequence is valid)
document_id = str

; Peer metadata exchanged during handshake
peer_metadata = {
    ? storageId: storage_id,    ; persistent storage identifier
    ? isEphemeral: bool          ; true if peer has no persistent storage
}
```

### 9.1 join

Sent by the initiating peer during the handshake phase.

```cddl
join = {
    type: "join",
    senderId: peer_id,
    supportedProtocolVersions: [+ protocol_version],
    peerMetadata: peer_metadata,
}
```

**Notes:**
- `supportedProtocolVersions` MUST contain at least one version. Currently only `"1"` is defined.
- `peerMetadata` is REQUIRED. A peer with no persistent storage SHOULD set `isEphemeral: true` and omit `storageId`.

### 9.2 peer

Sent by the receiving peer in response to a `join` message.

```cddl
peer = {
    type: "peer",
    senderId: peer_id,
    targetId: peer_id,
    selectedProtocolVersion: protocol_version,
    peerMetadata: peer_metadata,
}
```

**Notes:**
- `targetId` MUST match the `senderId` from the `join` message.
- `selectedProtocolVersion` MUST be a version that was present in the `join` message's `supportedProtocolVersions`.

### 9.3 error

Sent to indicate a protocol error. The sender MUST close the connection after sending this message.

```cddl
error = {
    type: "error",
    senderId: peer_id,
    targetId: peer_id,
    message: str,
}
```

**Notes:**
- The `message` field is a human-readable error description.

### 9.4 leave

An advisory message that a peer MAY send before it intentionally disconnects.

```cddl
leave = {
    type: "leave",
    senderId: peer_id,
}
```

**Notes:**
- Sending a `leave` message is OPTIONAL. Implementations MUST NOT depend on receiving a `leave` message -- disconnection is primarily detected via socket close events and keep-alive timeouts.

### 9.5 request

Sent when a peer wants to begin syncing a specific document. Functionally identical to `sync` but signals that the sender expects a `doc-unavailable` response if the recipient does not have the document.

```cddl
request = {
    type: "request",
    senderId: peer_id,
    targetId: peer_id,
    documentId: document_id,
    data: sync_message,
}
```

**Notes:**
- `data` contains the initial Automerge sync message (generated with a fresh sync state).
- This is the expected first message for a document the sender wants to obtain from the network.

### 9.6 sync

Sent to exchange Automerge sync data for a document.

```cddl
sync = {
    type: "sync",
    senderId: peer_id,
    targetId: peer_id,
    documentId: document_id,
    data: sync_message,
}
```

**Notes:**
- `data` MUST NOT be zero-length (see [Section 4.2](#42-framing)).
- The Automerge sync protocol is self-terminating: when both peers are in agreement, no further messages are sent for that document.

### 9.7 doc-unavailable

Sent to indicate that the sender does not have the requested document and none of its connected peers have it either.

```cddl
doc-unavailable = {
    type: "doc-unavailable",
    senderId: peer_id,
    targetId: peer_id,
    documentId: document_id,
}
```

### 9.8 ephemeral

Sent for application-level non-persisted messages (e.g., cursor positions, user presence).

```cddl
ephemeral = {
    type: "ephemeral",
    senderId: peer_id,
    targetId: peer_id,
    documentId: document_id,
    sessionId: str,
    count: uint,
    data: bstr,
}
```

**Notes:**
- `sessionId` is a random string unique to the sender's repo instance.
- `count` is a monotonically increasing counter per session, starting from 1.
- The combination of `(senderId, sessionId, count)` is used for deduplication: a recipient MUST discard any ephemeral message whose `count` is less than or equal to the highest `count` already seen for the same `(senderId, sessionId)` pair.
- `data` is arbitrary CBOR-encoded application data.

### 9.9 remote-subscription-change

Sent to modify the set of storage IDs the sender wants the recipient to monitor for remote heads changes.

```cddl
remote-subscription-change = {
    type: "remote-subscription-change",
    senderId: peer_id,
    targetId: peer_id,
    ? add: [* storage_id],     ; storage IDs to start watching
    ? remove: [* storage_id],  ; storage IDs to stop watching
}
```

**Notes:**
- Both `add` and `remove` are OPTIONAL. At least one SHOULD be present for the message to be meaningful.

### 9.10 remote-heads-changed

Sent to notify the recipient that a peer with a storage ID in the recipient's subscription has updated a document's heads.

```cddl
remote-heads-changed = {
    type: "remote-heads-changed",
    senderId: peer_id,
    targetId: peer_id,
    documentId: document_id,
    newHeads: {
        * storage_id => {
            heads: [* str],       ; base58check-encoded SHA-256 hashes
            timestamp: uint,      ; milliseconds since Unix epoch
        }
    },
}
```

---

## 10. Connection Maintenance

### 10.1 Dead Connection Detection

The server MUST detect and clean up dead or half-open connections (e.g., from network failures, NAT timeouts, or ungraceful client exits).

One possible mechanism is WebSocket-level ping/pong keep-alive:

1. The server sends a WebSocket ping frame to all connected clients at a regular interval (e.g., every 5 seconds).
2. Each client socket is tracked with an `isAlive` flag, initially `true`.
3. On each interval tick:
   - If `isAlive` is `true`: set it to `false` and send a WebSocket ping frame.
   - If `isAlive` is `false`: the client missed the previous ping -- terminate the connection.
4. When a pong frame is received from a client, set `isAlive` back to `true`.

Other mechanisms (e.g., OS-level TCP keepalive, transport-layer health monitoring) MAY be used instead, provided they achieve the same goal of timely detection and cleanup of dead connections.

---

## 11. Sync Server Behavior

This section describes the specific behavior expected of a sync server (as opposed to a general peer).

### 11.1 Role

A sync server is a long-running, always-available peer that:
- Accepts WebSocket connections from clients.
- Stores documents persistently.
- Relays sync messages between clients (when applicable).

### 11.2 Document Lifecycle on Server

1. **First request:** When a client sends a `request` for a document ID the server has never seen, the server creates a new empty document and begins syncing.
2. **Subsequent syncs:** The Automerge sync protocol handles merging changes from multiple clients.
3. **Persistence:** Documents SHOULD be saved to a persistent storage backend.
4. **Deletion:** Deleted documents SHOULD respond with `doc-unavailable`.

### 11.3 Sync State Persistence

For non-ephemeral peers (those with a `storageId`):
- After sync, implementations SHOULD persist the Automerge sync state.
- On reconnection, implementations SHOULD load persisted sync state.
- This allows efficient resynchronization (only exchanging changes since the last sync, rather than starting from scratch).

### 11.4 Concurrency

The Automerge sync protocol is designed for concurrent use -- multiple clients can sync the same document simultaneously. The CRDT properties of Automerge ensure that all peers converge to the same state regardless of message ordering.

However, implementations MUST ensure that sync message receive and generate operations for the same (document, peer) pair are serialized (not called concurrently) to maintain sync state consistency.

---

## 12. Remote Heads Gossiping

Remote heads gossiping is an OPTIONAL feature that allows peers to learn about the sync state of peers they are not directly connected to.

### 12.1 Use Case

In a topology like `Browser A <-> Sync Server <-> Browser B`, Browser A may want to know whether the sync server has received Browser B's latest changes (e.g., to show a "saved" indicator). Remote heads gossiping enables this without a direct connection between A and B.

### 12.2 Mechanism

1. A peer sends `remote-subscription-change` to add storage IDs it wants to monitor.
2. The receiving peer maintains a subscription list per connection.
3. When the receiving peer gets a `sync` message from a peer whose `storageId` is in a subscriber's list, AND the subscriber has access to that document, the receiving peer sends `remote-heads-changed` to the subscriber.
4. `remote-heads-changed` messages include a `timestamp` field. Recipients MUST only process messages with timestamps strictly greater than the last seen timestamp for the same `(storageId, documentId)` pair.
5. `remote-heads-changed` messages MAY be forwarded across multiple hops, as long as the forwarding peer performs the same access checks and timestamp filtering.

### 12.3 Heads Format

The `heads` field in `remote-heads-changed` contains an array of strings. Each string is a base58check-encoded SHA-256 hash representing an Automerge document head (change hash).

---

## 13. Security Considerations

### 13.1 Authentication

The base protocol does not define authentication. Implementations SHOULD add authentication at the transport layer (e.g., via an auth token in the WebSocket URL query string, HTTP headers during upgrade, or a custom message after handshake).

### 13.2 Authorization

The access policy (see [Section 8.2](#82-share-policy)) provides per-document access control. Servers MUST check access before processing sync messages. Unauthorized requests SHOULD receive a `doc-unavailable` response.

### 13.3 Transport Security

WebSocket connections SHOULD use TLS (`wss://`) in production.

### 13.4 Denial of Service

- Servers MAY implement connection limits per IP or per peer ID.
- The keep-alive mechanism ([Section 10.1](#101-keep-alive-pingpong)) helps reclaim resources from dead connections.
- Servers MAY implement rate limiting on incoming messages.

---

## Appendix A: Complete Message Type Summary

| Message Type | Direction | Phase | Required Fields |
|-------------|-----------|-------|-----------------|
| `join` | Client -> Server | Handshake | `type`, `senderId`, `supportedProtocolVersions`, `peerMetadata` |
| `peer` | Server -> Client | Handshake | `type`, `senderId`, `targetId`, `selectedProtocolVersion`, `peerMetadata` |
| `error` | Either | Any | `type`, `senderId`, `targetId`, `message` |
| `leave` | Either | Sync | `type`, `senderId` |
| `request` | Either | Sync | `type`, `senderId`, `targetId`, `documentId`, `data` |
| `sync` | Either | Sync | `type`, `senderId`, `targetId`, `documentId`, `data` |
| `doc-unavailable` | Either | Sync | `type`, `senderId`, `targetId`, `documentId` |
| `ephemeral` | Either | Sync | `type`, `senderId`, `targetId`, `documentId`, `sessionId`, `count`, `data` |
| `remote-subscription-change` | Either | Sync | `type`, `senderId`, `targetId` |
| `remote-heads-changed` | Either | Sync | `type`, `senderId`, `targetId`, `documentId`, `newHeads` |

## Appendix B: Example Message Exchange

```
# 1. WebSocket connection established

# 2. Client sends join (CBOR-encoded)
{
  "type": "join",
  "senderId": "client-abc123",
  "peerMetadata": {
    "storageId": "550e8400-e29b-41d4-a716-446655440000",
    "isEphemeral": false
  },
  "supportedProtocolVersions": ["1"]
}

# 3. Server sends peer (CBOR-encoded)
{
  "type": "peer",
  "senderId": "storage-server-myhost",
  "targetId": "client-abc123",
  "peerMetadata": {
    "storageId": "661f9511-f30c-52e5-b827-557766551111",
    "isEphemeral": false
  },
  "selectedProtocolVersion": "1"
}

# 4. Client requests a document (CBOR-encoded)
{
  "type": "request",
  "senderId": "client-abc123",
  "targetId": "storage-server-myhost",
  "documentId": "4NMNnkMhL8jXrdJ9jamS58PAVdXu",
  "data": <bytes: initial Automerge sync message>
}

# 5. Server responds with sync data (CBOR-encoded)
{
  "type": "sync",
  "senderId": "storage-server-myhost",
  "targetId": "client-abc123",
  "documentId": "4NMNnkMhL8jXrdJ9jamS58PAVdXu",
  "data": <bytes: Automerge sync response>
}

# 6. Sync continues until convergence...

# 7. If document not found:
{
  "type": "doc-unavailable",
  "senderId": "storage-server-myhost",
  "targetId": "client-abc123",
  "documentId": "4NMNnkMhL8jXrdJ9jamS58PAVdXu"
}
```

## Appendix C: Protocol Version History

| Version | Description |
|---------|-------------|
| `"1"` | Initial protocol version. All features described in this RFC. |

---

## References

- [Automerge](https://automerge.org/) -- The CRDT library underlying this protocol.
- [RFC 2119](https://datatracker.ietf.org/doc/html/rfc2119) -- Key words for use in RFCs to Indicate Requirement Levels.
- [RFC 6455](https://datatracker.ietf.org/doc/html/rfc6455) -- The WebSocket Protocol.
- [RFC 8610](https://datatracker.ietf.org/doc/html/rfc8610) -- CDDL (Concise Data Definition Language).
- [RFC 8949](https://datatracker.ietf.org/doc/html/rfc8949) -- CBOR (Concise Binary Object Representation).
- [Automerge Sync Protocol](https://automerge.org/docs/how-it-works/sync/) -- Description of the underlying sync algorithm.
