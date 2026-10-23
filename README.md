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
| `src/Server.elm` | The router as a pure state machine. Deadlines and rate meters are data, checked by a clock subscription that exists only while jobs do |
| `src/Commit.elm` | Create-only commit: `open(O_EXCL)` lease, write or stream, `fsync`, `link`, `unlink`, `fsync(dir)` |
| `src/Http/` | Request and response types; a bidirectional route codec with round-trip laws |
| `src/Io.elm`, `src/Verdict.elm` | Host primitives, host-error classification, and the failure tables that turn host errors into answers |
| `server.js` | The bridge: HTTP front end, resource table, and the transaction interpreter |
| `upstream.js` | A deliberately hostile mock upstream for `/delayed/:id` |

## Routes

| Route | Behaviour |
| --- | --- |
| `GET /health`, `GET /items/:id`, `GET /stats` | Answered from Elm state |
| `POST /echo` | Reflects the exact JSON bytes sent |
| `GET /delayed/:id` | Fetches the item from the upstream over real HTTP |
| `POST /ingest/:id` | Commits a JSON object to `file_<id>.final`, create-only, returns its sha256 |
| `PUT /blobs/:id` | Streams the body to disk and commits it to `blob_<id>.bin`, create-only |
| `GET /blobs/:id` | Streams a committed blob back with an exact Content-Length |

## Running

```sh
npm install
npm test              # Elm laws and model tests, a differential test of the JS
                      # interpreter against the Elm reference, then the real-disk suite
node upstream.js &    # the mock upstream on :3001
npm start             # the server on :3000, data in ./data
npm run bench         # what carrying binary through Elm ports costs, by strategy
```

Tested on Node 26 on Linux. The acceptance suite uses `/proc/self/fd`, `mkfifo`
and `chmod 000`, so it needs Linux and a non-root user. It writes to
`.test-data/` in the repository rather than `/tmp`, because `/tmp` is often
tmpfs, where `fsync` is a no-op.
