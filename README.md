# hespot

**A clean-room Spotify client core, written from scratch in Haskell.**

hespot speaks Spotify's access-point protocol — the same one the Rust
[`librespot`](https://github.com/librespot-org/librespot) project speaks — but every
byte is implemented from scratch: the Shannon stream cipher, the Diffie–Hellman
handshake, the protobuf codec, the Mercury bus, the audio pipeline, and the modern
client-token / login5 token stack. librespot is used **only** as a protocol reference;
no code is copied.

The result logs into a real Spotify account, streams and downloads tracks as tagged
Ogg Vorbis (or transcoded MP3 / FLAC / WAV with cover art), plays them, and mints the
modern tokens today's Spotify backend expects.

> **Educational / interoperability project.** Use your own account. Streaming needs
> Premium, exactly like librespot. Not affiliated with Spotify.

---

## Highlights

- **Pure-Haskell crypto & wire formats** — Shannon (bit-exact vs. the original crate's
  vectors), Diffie–Hellman, the RSA server-signature (anti-MITM) check, and a
  hand-rolled protobuf codec. No FFI, no bindings to librespot.
- **Downloads that beat librespot's export** — choose quality (320 / 160 / 96) and
  format (Ogg / MP3 / FLAC / WAV), embed cover art and rich tags (ISRC, UPC, release
  date, label…), auto-name files, and grab a whole album in one command.
- **From-scratch Ogg Vorbis tagging** — including `METADATA_BLOCK_PICTURE` cover
  embedding, which ffmpeg refuses to do for the Ogg muxer, via a hand-written Ogg
  repaginator + CRC32.
- **The modern token stack** — client-token, a hash-cash proof-of-work solver, and
  login5, all built from the protobufs and verified live. This is what unlocks today's
  spclient / partner endpoints.
- **Verified, not vibes** — a vector/property test-suite, live runs against Spotify,
  and real-debugger sessions (GHCi, `protoc --decode_raw`, strace, ffprobe).

---

## Quick start

```sh
# 1. Build  (needs GHC 9.6 + cabal 3, e.g. via ghcup)
cabal build

# 2. Log in once — opens your browser, caches a reusable credential blob
cabal run hespot -- oauth-login

# 3. Download a track — tagged, with cover art, auto-named "Artist - Title.ogg"
cabal run hespot -- download 0VjIjW4GlUZAMYd2vXMi3b auto

# 4. …or just play it
cabal run hespot -- play 0VjIjW4GlUZAMYd2vXMi3b
```

A `<uri>` is anything that identifies a track: `spotify:track:0VjI…`, an
`https://open.spotify.com/track/0VjI…` URL, or the bare base-62 id.

---

## Commands

| Command | What it does |
| --- | --- |
| `oauth-login` | Browser (PKCE) login, then cache credentials |
| `login-cached` | Reuse the cached credential blob (no browser) |
| `login <token>` | Log in with an access token you already have |
| `whoami` | Account info: username, country, product, profile |
| `track <uri>` | Print full metadata + the available audio files |
| `download <uri> <out\|dir\|auto> [flags]` | Download one track |
| `album <album-uri> <dir> [flags]` | Download a whole album into a folder |
| `play <uri> [--quality …]` | Stream and play through ffplay / mpv |
| `lyrics <uri> [out.lrc]` | Fetch synced lyrics ¹ |
| `client-token` | Mint a client-token (solves the hash-cash challenge) |
| `login5` | Mint a login5 access token from cached credentials |
| `connect` | Appear as a Spotify Connect device in the official app |
| `handshake` | Run just the crypto handshake (no account needed) |

**Download flags:**&nbsp; `--quality 320｜160｜96｜flac` &nbsp;·&nbsp; `--format ogg｜mp3｜flac｜wav` &nbsp;·&nbsp; `--no-cover`

<sub>¹ `lyrics` exercises the spclient color-lyrics endpoint, which Spotify gates to
lyrics-entitled clients — expect a 403 with a stock token. The request chain itself
is complete.</sub>

### Examples

```sh
# Highest quality, native Ogg, fully tagged + cover embedded, auto-named
cabal run hespot -- download 0VjIjW4GlUZAMYd2vXMi3b auto --quality 320

# Transcode to MP3 with an attached cover-art frame
cabal run hespot -- download 0VjIjW4GlUZAMYd2vXMi3b "Blinding Lights.mp3" --format mp3

# A whole album, every track tagged, into ./music
cabal run hespot -- album spotify:album:4yP0hdKOZPNshxUOjY0cZj ./music

# Who am I?
cabal run hespot -- whoami
#   Username : <your-account-id>
#   Country  : CZ   (pushed by the AP)
#   Product  : premium

# Mint the modern tokens (foundation for spclient / partner endpoints)
cabal run hespot -- client-token   #  client-token OK: AAH…  (332 chars)
cabal run hespot -- login5         #  login5 access token OK: BQ…  (438 chars)
```

---

## How it works

```
oauth-login ─▶ DH + Shannon handshake ─▶ encrypted, MAC'd session
                                                │
        ┌───────────────────────────────────────┼─────────────────────────────┐
        ▼                                        ▼                             ▼
   Mercury bus                          audio-key request                modern tokens
 hm://metadata/4/track                 (AP RequestKey 0x0c)          client-token + login5
        │                                        │                    (hash-cash solver)
        ▼                                        ▼
   TrackInfo  ───────────────▶  chunked channel fetch ─▶ AES-128-CTR ─▶ strip 167-byte
 (title, artists, ISRC,           (128 KiB chunks)         decrypt        Ogg header
  cover id, files, …)                                                         │
                                                                             ▼
                                            native Ogg retag (cover + tags) ──┐
                                                                              ├──▶ file
                                                   or ffmpeg transcode  ──────┘
```

1. **Handshake** — resolve an access point, send a `ClientHello` (DH public key), verify
   the RSA server signature (anti-MITM), and derive Shannon keys. Every later packet is
   Shannon-encrypted and MAC'd.
2. **Session** — a background thread routes packets: ping→pong, country / product,
   Mercury responses, audio keys, and channel data.
3. **Metadata** — fetched over the Mercury bus (`hm://metadata/4/track/<hex>`) and parsed
   by the hand-rolled protobuf decoder.
4. **Audio** — request the per-file AES key over the AP channel, fetch the file in 128 KiB
   chunks, AES-128-CTR decrypt, then strip Spotify's 167-byte Ogg header page → real Ogg
   Vorbis.
5. **Output** — retag the Ogg natively (cover + tags) or transcode via ffmpeg.
6. **Modern tokens** — solve a hash-cash challenge at `clienttoken.spotify.com` for a
   client-token, and exchange the cached stored-credential blob at `login5.spotify.com`
   for a login5 access token — the credentials today's endpoints expect.

---

## Using it as a library

```haskell
import Spotify
import Spotify.Auth.Cache (defaultCachePath, loadCredentials)
import Spotify.Id         (parseTrackUri)
import Spotify.Metadata   (fetchTrack, tiName, tiArtists)

main :: IO ()
main = do
  -- Reuse the blob cached by `oauth-login` (no browser this time):
  Just creds <- loadCredentials =<< defaultCachePath
  session    <- connect defaultConfig creds

  case parseTrackUri "spotify:track:0VjIjW4GlUZAMYd2vXMi3b" of
    Left err  -> putStrLn err
    Right sid -> do
      Right track <- fetchTrack session sid
      print (tiName track, tiArtists track)

  disconnect session
```

---

## Verification

- **Test-suite** — `cabal test` checks Shannon against the Rust crate's vectors,
  AES-CTR against NIST SP 800-38A, base62 against librespot, HMAC against RFC 2202,
  PKCE against RFC 7636, and the hash-cash difficulty.
- **Live** — every feature is run against Spotify's real servers before it is called done.
- **Real-debugger discipline** — GHCi breakpoints, `protoc --decode_raw` on the actual
  request/response protobufs, and strace / ffprobe — verifying behaviour at runtime
  rather than by reading code.

---

## Building

```sh
cabal build      # build the library + CLI
cabal test       # run the crypto / protobuf test-suite
cabal run hespot -- <command>
```

Requires **GHC 9.6** and **cabal 3** (e.g. via [`ghcup`](https://www.haskell.org/ghcup/)).
Transcoding and `play` use **ffmpeg** / **ffplay** if present.

See [`DESIGN.md`](./DESIGN.md) for the architecture and detailed protocol notes.
