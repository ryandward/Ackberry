# Ackberry

A headless HTTP server in which every decision is made in pure Elm 0.19.1. Node
is reduced to a socket front end and an interpreter for a small transaction
algebra; it routes nothing, chooses no status codes, and never decides what an
error means.

Request bodies are never buffered on Elm's behalf. A request reaches Elm when its
headers are parsed; its body stays on the socket until a transaction step reads
it into memory (small JSON bodies, under a limit Elm sets) or pipes it straight
to a leased file on disk. Elm holds every transaction as a capability it can
abort, and watches every streaming step through a rate meter.

## Map

| File | What it is |
| --- | --- |
| `src/Tx.elm` | The transaction algebra (`Do op cleanups`, `Commit op`, `Always`/`OnFailure` cleanups, `Pause`/`Finish`, abort), its seven laws, and a pure reference interpreter |
| `src/Plan.elm` | Typestate builder: the only way the server can construct a transaction. A lease without its cleanups, a commit before fsync, or a write into an unleased file does not compile |
| `src/Server.elm` | The router as a pure state machine. Deadlines and rate meters are data, checked by a clock subscription that exists only while jobs do. Also the janitor, which prepares the data directory before the server listens and collects the parts crashed processes leave |
| `src/Layout.elm` | Every path the server touches. In-flight bytes live in `.tmp/<transaction>.part`, named after the transaction and never after the resource it will become |
| `src/Commit.elm` | Create-only commit: `open(O_EXCL)` an anonymous part, write or stream, `fsync`, `link` it to the final name, `unlink` the part, `fsync` the final's directory |
| `src/Http/` | Request and response types; a bidirectional route codec with round-trip laws |
| `src/Io.elm`, `src/Verdict.elm` | Host primitives, host-error classification, and the failure tables that turn host errors into answers |
| `server.js` | The bridge: HTTP front end, resource table, the transaction interpreter, and the staged close |
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

## Refusals

A request refused before its body was read (413 from the headers, 400, 404,
405, 408, 501) ends its connection. Closing at once loses answers: the client
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
npm test              # Elm laws and model tests, a differential test of the JS
                      # interpreter against the Elm reference, the real-disk
                      # suite, then SIGKILL against the real process
node upstream.js &    # the mock upstream on :3001
npm start             # the server on :3000, data in ./data (it creates
                      # ./data/.tmp and ./data/blobs before it listens)
npm run bench         # what carrying binary through Elm ports costs, by strategy
```

Tested on Node 26 on Linux. The acceptance suite uses `/proc/self/fd`, `mkfifo`
and `chmod 000`, so it needs Linux and a non-root user. It writes to
`.test-data/` in the repository rather than `/tmp`, because `/tmp` is often
tmpfs, where `fsync` is a no-op.
