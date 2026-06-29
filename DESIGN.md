# Design notes

## Goals

1. **Clean-room.** Implement the Spotify access-point protocol from the wire spec.
   The Rust `librespot` is consulted only to learn *what* the protocol is and *what*
   the public API should feel like (its `examples/`). No code is ported verbatim.
2. **Small & legible.** Few dependencies, explicit data flow, no macro/codegen magic.
3. **Hard to misuse.** Strong types at the edges; the crypto state machine is hidden.

## Module map

```
Spotify.Proto.Wire            -- tiny protobuf (wire) encoder/decoder
Spotify.Proto.Keyexchange     -- ClientHello / APResponse / ClientResponsePlaintext   (later)
Spotify.Proto.Authentication  -- ClientResponseEncrypted / APWelcome                   (later)

Spotify.Crypto.Shannon        -- Shannon stream cipher + MAC (the AP channel cipher)
Spotify.Crypto.DiffieHellman  -- DH(768-bit MODP) keypair + shared secret
Spotify.Crypto.Keys           -- handshake key derivation + RSA server-signature check

Spotify.Net.ApResolve         -- resolve an access point over HTTPS                     (later)
Spotify.Connection            -- TCP + handshake + Shannon-framed packet channel        (later)
Spotify.Auth                  -- credentials + login over the encrypted channel         (later)
Spotify.Session               -- high-level: connect + authenticate -> Session          (later)
Spotify                       -- the public, friendly facade                            (later)
```

## The handshake (what milestone 1 implements)

1. **Resolve** an access point host:port from `https://apresolve.spotify.com`.
2. **TCP connect**, then a Diffie–Hellman handshake:
   * Send `ClientHello` — framed as `00 04 | u32be totalSize | protobuf`.
   * Receive `APResponseMessage`  — framed as `u32be totalSize | protobuf`.
   * **Verify** the server's `gs` against `gs_signature` with Spotify's well-known
     2048-bit RSA key (prevents man-in-the-middle). `e = 65537`.
   * Compute `shared = gs ^ priv mod p`.
   * Derive keys with HMAC-SHA1:
     `data = ∥_{i=1..5} HMAC(shared, packets ‖ i)` (100 bytes);
     `challenge = HMAC(data[0:20], packets)`;
     `send_key = data[20:52]`, `recv_key = data[52:84]`.
   * Send `ClientResponsePlaintext{ hmac = challenge }`, framed as `u32be size | protobuf`.
3. From here the socket is a **Shannon-encrypted packet channel**. Each packet is
   `cmd:u8 | len:u16be | payload`, encrypted, followed by a 4-byte MAC. A fresh
   nonce (a monotonically increasing `u32`, separate counters per direction) keys
   the cipher for every packet.

## Authentication (next)

Send a `Login` (`0xab`) packet carrying `ClientResponseEncrypted`:
`login_credentials{ username?, typ, auth_data } + system_info + version_string`.
The server replies with `APWelcome` (`0xac`) — which contains a **reusable
credentials blob** to cache — or `AuthFailure` (`0xad`).

`typ` is one of: `AUTHENTICATION_SPOTIFY_TOKEN` (an OAuth access token),
`AUTHENTICATION_STORED_SPOTIFY_CREDENTIALS` (the cached blob), or the legacy
user/pass.

## Protocol constants (verified against the reference)

* DH generator `2`, 768-bit MODP prime (RFC 2409 group 1's prime tail pattern).
* DH private key = 95 random bytes interpreted **little-endian**.
* Shannon: `N=16`, `KEYP=13`, `INITKONST=0x6996c53a`.
* `SPOTIFY_VERSION = 124200290`.
* Packet types: `Login=0xab`, `APWelcome=0xac`, `AuthFailure=0xad`,
  `Ping=0x04`, `Pong=0x49`, `CountryCode=0x1b`, `ProductInfo=0x50`.

## Why hand-rolled protobuf?

The auth path needs only ~8 messages. A ~120-line wire codec keeps the dependency
footprint tiny and the "from scratch" spirit intact. If the metadata/Connect
surface grows, swapping in code generation behind the same typed layer is easy.
