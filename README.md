# Ackberry

A headless HTTP server in which every decision is made in pure Elm 0.19.1. Node
is reduced to raw TCP sockets (`node:net`, not `node:http`) and an interpreter
for a small transaction algebra; it parses nothing it could decide, routes
nothing, chooses no status codes, keeps no deadlines of its own, and never
decides what an error means. Elm parses every request head and writes every
byte of every response. Why `node:http` had to go is in `ARCHITECTURE.md`.

Request bodies are never buffered on Elm's behalf. A request reaches Elm as the
bytes of its head; its body stays on the socket until a transaction step reads
it into memory (small JSON bodies, under a limit Elm sets) or pipes it straight
to a leased file on disk. Elm holds every transaction as a capability it can
abort, and watches every streaming step through a rate meter.

## Map

| File | What it is |
| --- | --- |
| `src/Tx.elm` | The transaction algebra (`Do op cleanups`, `Commit op`, `Always`/`OnFailure` cleanups, `Pause`/`Finish`, abort), its seven laws, and a pure reference interpreter |
| `src/Plan.elm` | Typestate builder: the only way the server can construct a transaction. A lease without its cleanups, a commit before fsync, or a write into an unleased file does not compile |
| `src/Server.elm` | The server as a pure state machine: connections, exchanges, routing, deadlines and rate meters as data, checked against the bridge's clock. Also the janitor, which prepares the data directory before the server listens and collects the parts crashed processes leave |
| `src/Kernel.elm` | How a connection whose body was never read is closed, and the pool of connections that may linger while they are |
| `src/Layout.elm` | Every path the server touches. In-flight bytes live in `.tmp/<transaction>.part`, named after the transaction and never after the resource it will become |
| `src/Commit.elm` | Create-only commit: `open(O_EXCL)` an anonymous part, write or stream, `fsync`, `link` it to the final name, `unlink` the part, `fsync` the final's directory |
| `src/Http/Head.elm` | The request head under RFC 9112: request line, fields, `Host`, framing, persistence, `Expect`, every refusal named |
| `src/Http/Response.elm`, `src/Http/Date.elm` | Responses, serialized byte for byte: `Content-Length` in UTF-8 bytes, an IMF-fixdate `Date`, `100 Continue` |
| `src/Http/Chunked.elm` | The chunked coding's reference semantics, which the bridge's decoder must match byte for byte |
| `src/Http/Request.elm`, `src/Http/Route.elm` | Requests, connection and request ids; a bidirectional route codec with round-trip laws |
| `src/Io.elm`, `src/Verdict.elm` | Host primitives, host-error classification, and the failure tables that turn host errors into answers |
| `server.js` | The bridge: sockets, head delimiting, the chunked decoder, the resource table, the transaction interpreter, and the staged close |
| `upstream.js` | A deliberately hostile mock upstream for `/delayed/:id` |

## Routes

| Route | Behaviour |
| --- | --- |
| `GET /health`, `GET /items/:id`, `GET /stats` | Answered from Elm state |
| `POST /echo` | Reflects the exact JSON bytes sent |
| `GET /delayed/:id` | Fetches the item from the upstream over real HTTP |
| `POST /ingest/:id` | Commits a JSON object to `file_<id>.final`, create-only, returns its sha256 |
| `PUT /blobs/:id` | Streams the body into an anonymous part and commits it to `blobs/<id>`, create-only |
| `GET /blobs/:id` | Streams `blobs/<id>` back with an exact Content-Length |

## Connections

A connection carries one exchange at a time. Its first head must arrive
within `headDeadlineMs` (10 s) of the accept, or Elm answers 408 and closes;
between requests it may sit idle for `idleMs` (5 s), after which it is closed
without a word. A connection persists after a response only if the request's
body was read to its end, the client did not ask to close, the client has not
ended its side, and the server is still serving. Requests are numbered on
their connection (`<connection>.<n>`), and pipelined requests wait on the
socket, unparsed, until Elm has answered the one before.

Every deadline is Elm's, on the monotonic clock the bridge stamps on each
event; Elm keeps no timers and asks for one alarm at a time.

A head the bridge cannot delimit within `maxHeadBytes` (16 KiB) is 431. A
head Elm refuses is answered and the connection closed: 400 for a malformed
line or field, a missing, repeated or invalid `Host`, or body framing that is
ambiguous or malformed; 414 for a long target; 417 for an expectation other than
`100-continue`; 501 for an unknown method or transfer coding; 505 for a
major version other than 1.

## Refusals

A request refused before its body was read (413 from the headers, 400, 404,
405, 408, 414, 417, 431, 501, 505) ends its connection. Closing at once loses answers: the client
is still sending, the kernel answers its next bytes with a reset, and a client
whose send fails that way can give up without reading the response already in
its receive queue (RFC 9112 section 9.6). So the bridge closes in stages,
within bounds Elm sets: it sends the response and a FIN, then reads and drops
the client's bytes, never parsing them, until the client hangs up, 500 ms
pass, or 128 KiB have been dropped. At most 64 connections linger at once;
past that, the close is immediate. `/stats` counts how each one ended.

## Crashes

A process killed mid-upload (SIGKILL, the OOM killer, a power cut) leaves its
part in `<data>/.tmp/`. The part is named after the dead transaction, so it
blocks nothing: a retry of the same id streams into its own part and commits.
Uploads to one id never exclude each other; they race at the commit, where
`link(2)` lets exactly one win, so a 409 means exactly one thing: the id is
already committed. `tests/crash.js` kills the real process mid-upload and
checks all of this.

The commit is `link` then `unlink`, not `rename`: with nothing excluding two
uploads to one id, `rename` would let the second silently replace the first
after both clients were told 201.

Corpses are collected by the janitor: at boot and then every `sweepEveryMs`
(15 minutes by default), it removes each regular file in `.tmp/` that is named
like a part, is not held by a live transaction, and was last written more than
`staleAfterMs` (2 hours by default) before the host listed the directory. The
server refuses to start unless `staleAfterMs` exceeds `transferDeadlineMs`.

## Running

```sh
npm install
npm test              # Elm laws and model tests, differential tests of the JS
                      # interpreter and the JS chunked decoder against their
                      # Elm references, the real-disk suite, then SIGKILL
                      # against the real process
node upstream.js &    # the mock upstream on :3001
npm start             # the server on :3000, data in ./data (it creates
                      # ./data/.tmp and ./data/blobs before it listens)
npm run bench         # what carrying binary through Elm ports costs, by strategy
```

Tested on Node 26 on Linux. The acceptance suite uses `/proc/self/fd`, `mkfifo`
and `chmod 000`, so it needs Linux and a non-root user. It writes to
`.test-data/` in the repository rather than `/tmp`, because `/tmp` is often
tmpfs, where `fsync` is a no-op.
