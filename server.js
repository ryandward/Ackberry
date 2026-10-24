'use strict';

// The bridge: a TCP front end, a table of host resources, and an
// interpreter for the transaction algebra defined in src/Tx.elm.
//
// It decides nothing. Every connection is Elm's from its first byte: the
// bridge reports what happens on a socket, and carries out the ordered
// array of instructions Elm sends per update:
//
//   awaitHead  read until the end of a head, within Elm's byte limit
//   send       write bytes Elm serialized
//   close      FIN, then close once the bytes are flushed
//   dissipate  FIN, then drop the client's bytes within Elm's bounds, then close
//   begin      start a transaction: an ordered list of steps
//   extend     append steps to a paused transaction
//   abort      preempt a transaction
//   alarm      wake Elm once, after a delay, or never
//   listen     start accepting connections
//   note, log  observation: a request line, a fault
//   halt       Elm refuses to serve; say why
//
// Every event it reports carries the host's two clocks, read as the event
// goes out: performance.now() for deadlines, Date.now() for dates. It keeps
// no time of its own except the bounds Elm hands a single dissipation.
//
// The interpreter knows sequencing, a cleanup stack and a commit flag; it
// does not know what any operation means. The drivers know how to perform
// one operation each and how to sever it when its AbortSignal fires; they do
// not know what a transaction is. tests/vm checks the interpreter against
// Elm's reference semantics on random programs, and tests/chunked checks the
// chunked decoder against Elm's reference decoder.

const http = require('node:http');
const net = require('node:net');
const fs = require('node:fs/promises');
const { constants: fsConstants } = require('node:fs');
const path = require('node:path');
const { createHash, randomUUID } = require('node:crypto');
const { EventEmitter } = require('node:events');
const { Readable, Transform, Writable } = require('node:stream');
const { pipeline } = require('node:stream/promises');
const { Elm } = require('./build/main.js');

// fatal: invalid UTF-8 becomes an explicit tag instead of silent U+FFFD.
// ignoreBOM: keep a leading BOM, so Elm sees exactly what was sent.
const utf8 = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// The progress contract (a validity bound, in the sense of L-spec Theorem 16):
// every byte a streaming step moves is reported, with the time of the report,
// within this many milliseconds. Elm gets the number as a flag and never
// judges a count before the bound has passed.
const PROGRESS_EVERY_MS = 100;
const now = () => process.hrtime.bigint();

// Host errors cross the port as data: every field copied, none interpreted,
// the cause chain kept. Elm decides what a code means.
function hostError(err) {
  return {
    name: err?.name ?? null,
    code: err?.code ?? null,
    syscall: err?.syscall ?? null,
    rawMessage: String(err?.message ?? err),
    cause: err?.cause == null ? null : hostError(err.cause),
  };
}

const fault = (name, code, message) => Object.assign(new Error(message), { name, code });

// Streams over a FileHandle, built on filehandle.read/write. Not
// filehandle.createReadStream/createWriteStream: with autoClose: false those
// take a reference on the handle and never release it, so filehandle.close()
// waits forever. Streaming on the raw fd number instead would be worse: a
// close racing a pending threadpool write lets the kernel reuse the number,
// and the write lands in another file. Here every read and write holds its
// own reference, so close() waits for in-flight I/O and nothing else.
function fileSink(fh) {
  let position = 0;
  return new Writable({
    highWaterMark: 64 * 1024,
    write(chunk, _encoding, done) {
      const rest = async () => {
        let offset = 0;
        while (offset < chunk.length) {
          const { bytesWritten } = await fh.write(chunk, offset, chunk.length - offset, position + offset);
          offset += bytesWritten;
        }
        position += chunk.length;
      };
      rest().then(() => done(), done);
    },
  });
}

function fileSource(fh, length) {
  let position = 0;
  return new Readable({
    highWaterMark: 64 * 1024,
    read(size) {
      const want = Math.min(size, length - position);
      if (want <= 0) return this.push(null);
      const buffer = Buffer.allocUnsafe(want);
      return fh.read(buffer, 0, want, position).then(({ bytesRead }) => {
        if (bytesRead === 0) return this.destroy(fault('Truncated', null, `the file ended at ${position} of ${length} bytes`));
        position += bytesRead;
        return this.push(bytesRead === want ? buffer : buffer.subarray(0, bytesRead));
      }, (err) => this.destroy(err));
    },
  });
}

// ---------------------------------------------------------------------------
// The interpreter. See the laws in src/Tx.elm.

function createInterpreter({ drivers, report, progress, reclaim, events }) {
  const txs = new Map();
  const NEVER = new AbortController().signal;

  async function attempt(t, op, signal) {
    events.emit('io', { phase: 'start', request: t.request, kind: op.kind, at: now() });
    let outcome;
    try {
      const ok = await drivers[op.kind](op, {
        signal,
        request: t.request,
        opened: (handle) => t.opened.add(handle),
        progress: (bytes, running, at) => progress(t.request, bytes, running, at),
      });
      outcome = { ok: ok ?? {} };
    } catch (err) {
      outcome = { error: hostError(err) };
    }
    events.emit('io', { phase: 'end', request: t.request, kind: op.kind, outcome, at: now() });
    return outcome;
  }

  function flush(t, status, leaked = []) {
    report(t.request, {
      status,
      committed: t.committed,
      aborted: t.aborted,
      steps: t.results.splice(0),
      cleanups: status === 'settled' ? t.cleanupResults : [],
      leaked,
    });
  }

  async function drive(t) {
    for (;;) {
      if (t.queue.length === 0) {
        if (t.more && !t.aborted) {
          // Arm the wake-up before reporting: Elm may answer the report with
          // an extend synchronously, inside the report call itself.
          const resumed = new Promise((resolve) => { t.wake = resolve; });
          flush(t, 'paused');
          await resumed;
          t.wake = null;
          continue;
        }
        break;
      }
      if (t.aborted && !t.committed) break;
      const step = t.queue.shift();
      const controller = new AbortController();
      t.running = { controller, shielded: t.committed };
      const outcome = await attempt(t, step.op, controller.signal);
      t.running = null;
      t.results.push(outcome);
      if ('ok' in outcome) {
        if (step.step === 'commit') t.committed = true;
        else t.cleanups.push(...step.cleanups);
      } else if (!t.committed) {
        break;
      }
    }
    const due = t.cleanups.filter((cleanup) => cleanup.when === 'always' || !t.committed).reverse();
    for (const cleanup of due) {
      const outcome = await attempt(t, cleanup.op, NEVER);
      t.cleanupResults.push({ kind: cleanup.op.kind, path: cleanup.op.path ?? null, outcome });
    }
    const leaked = await reclaim(t.opened);
    txs.delete(t.request);
    events.emit('tx', { phase: 'settled', request: t.request, committed: t.committed, aborted: t.aborted, at: now() });
    flush(t, 'settled', leaked);
  }

  return {
    txs,
    begin({ tx, steps, then }) {
      const t = {
        request: tx,
        queue: [...steps],
        more: then === 'pause',
        cleanups: [],
        committed: false,
        aborted: false,
        running: null,
        wake: null,
        results: [],
        cleanupResults: [],
        opened: new Set(),
      };
      txs.set(tx, t);
      drive(t);
    },
    extend({ tx, steps, then }) {
      const t = txs.get(tx);
      if (t === undefined || t.wake === null) return; // only a paused transaction can be extended
      t.queue.push(...steps);
      t.more = then === 'pause';
      t.wake();
    },
    abort({ tx }) {
      const t = txs.get(tx);
      if (t === undefined || t.aborted) return;
      t.aborted = true;
      events.emit('tx', { phase: 'abort', request: tx, at: now() });
      if (t.running !== null && !t.running.shielded) t.running.controller.abort();
      if (t.wake !== null) t.wake(); // a paused transaction ends: unwound, or finished if committed
    },
  };
}

// fsync on a directory: makes its entries, not the files they name, durable.
async function syncDirectory(dirPath) {
  const dir = await fs.open(dirPath, 'r');
  try {
    await dir.sync();
  } finally {
    await dir.close();
  }
}

// ---------------------------------------------------------------------------
// The chunked transfer coding (RFC 9112 section 7.1), decoded mechanically.
// It decides nothing a chunked body could mean; it only finds where one
// ends, and it refuses framing it cannot read rather than guess: sizes are
// 1*HEXDIG, only whitespace before a ';' may follow them, every line ends in
// CRLF, a size line or the trailer section past its limit is refused. It
// must agree byte for byte with src/Http/Chunked.elm, which tests/chunked
// checks on random and corrupted streams split at random places.

const framing = (offset, why) => Object.assign(fault('FramingError', 'ACKBERRY_FRAMING', `${why} at byte ${offset}`), { offset });
const isHex = (b) => (b >= 0x30 && b <= 0x39) || (b >= 0x41 && b <= 0x46) || (b >= 0x61 && b <= 0x66);
const hexValue = (b) => (b <= 0x39 ? b - 0x30 : (b | 0x20) - 0x61 + 10);
const isLineByte = (b) => b === 0x09 || (b >= 0x20 && b !== 0x7f);

class ChunkedDecoder {
  constructor({ maxLine, maxTrailer }) {
    this.maxLine = maxLine;
    this.maxTrailer = maxTrailer;
    this.state = 'size';
    this.size = 0;
    this.digits = 0;
    this.line = 0;
    this.trailer = 0;
    this.remaining = 0;
    this.offset = 0; // bytes consumed so far, for error positions
  }

  // Consumes from `buf`. Returns the data found, how many bytes of `buf`
  // were used, and whether the body ended; bytes after the end are not used.
  push(buf) {
    const data = [];
    let i = 0;
    while (i < buf.length && this.state !== 'done') {
      const at = this.offset + i;
      if (this.state === 'data') {
        const n = Math.min(this.remaining, buf.length - i);
        data.push(buf.subarray(i, i + n));
        this.remaining -= n;
        i += n;
        if (this.remaining === 0) this.state = 'dataCR';
        continue;
      }
      const b = buf[i];
      i += 1;
      // What the limits count, judged by the state the byte was read in: a
      // size line's bytes up to its CR, and every byte of the trailer
      // section through the final LF.
      const inLine = (this.state === 'size' || this.state === 'bws' || this.state === 'ext') && b !== 0x0d;
      const inTrailer = this.state === 'trailer' || this.state === 'trailerLine' || this.state === 'trailerLF' || this.state === 'endLF';
      switch (this.state) {
        case 'size':
          if (isHex(b)) {
            this.digits += 1;
            if (this.digits > 13) throw framing(at, 'a chunk size longer than 13 hex digits');
            this.size = this.size * 16 + hexValue(b);
          } else if (this.digits === 0) {
            throw framing(at, 'a chunk size that does not start with a hex digit');
          } else if (b === 0x3b) {
            this.state = 'ext';
          } else if (b === 0x20 || b === 0x09) {
            this.state = 'bws';
          } else if (b === 0x0d) {
            this.state = 'sizeLF';
          } else {
            throw framing(at, 'a chunk size followed by something other than an extension or CRLF');
          }
          break;
        case 'bws':
          if (b === 0x3b) this.state = 'ext';
          else if (b !== 0x20 && b !== 0x09) throw framing(at, 'whitespace after a chunk size that no extension follows');
          break;
        case 'ext':
          if (b === 0x0d) this.state = 'sizeLF';
          else if (!isLineByte(b)) throw framing(at, 'a control character in a chunk extension');
          break;
        case 'sizeLF':
          if (b !== 0x0a) throw framing(at, 'a chunk size line that does not end in CRLF');
          if (this.size === 0) {
            this.state = 'trailer';
          } else {
            this.state = 'data';
            this.remaining = this.size;
          }
          this.size = 0;
          this.digits = 0;
          this.line = 0;
          break;
        case 'dataCR':
          if (b !== 0x0d) throw framing(at, 'chunk data longer than its size');
          this.state = 'dataLF';
          break;
        case 'dataLF':
          if (b !== 0x0a) throw framing(at, 'chunk data not followed by CRLF');
          this.state = 'size';
          break;
        case 'trailer':
          if (b === 0x0d) {
            this.state = 'endLF';
          } else if (isLineByte(b)) {
            this.state = 'trailerLine';
          } else {
            throw framing(at, 'a control character in the trailer section');
          }
          break;
        case 'trailerLine':
          if (b === 0x0d) this.state = 'trailerLF';
          else if (!isLineByte(b)) throw framing(at, 'a control character in the trailer section');
          break;
        case 'trailerLF':
          if (b !== 0x0a) throw framing(at, 'a trailer line that does not end in CRLF');
          this.state = 'trailer';
          break;
        case 'endLF':
          if (b !== 0x0a) throw framing(at, 'a chunked body that does not end in CRLF');
          this.state = 'done';
          break;
        default:
          throw framing(at, `the decoder reached ${this.state}`);
      }
      if (inLine) {
        this.line += 1;
        if (this.line > this.maxLine) throw framing(at, 'a chunk size line past its limit');
      }
      if (inTrailer) {
        this.trailer += 1;
        if (this.trailer > this.maxTrailer) throw framing(at, 'a trailer section past its limit');
      }
    }
    this.offset += i;
    return { data, used: i, done: this.state === 'done' };
  }
}

// ---------------------------------------------------------------------------
// Connections. The bridge reads ahead of Elm by at most READ_AHEAD bytes, so
// a peer's FIN or reset is seen promptly, and beyond that only while a
// reader Elm installed (a head, a body, a dissipation) wants bytes; past
// that the socket is paused and TCP's own flow control holds the client.

const READ_AHEAD = 64 * 1024;
const CRLF = Buffer.from('\r\n', 'latin1');
const END_OF_HEAD = Buffer.from('\r\n\r\n', 'latin1');

// The first `n` queued bytes, removed from the queue.
function take(conn, n) {
  const parts = [];
  let left = n;
  while (left > 0) {
    const first = conn.queue[0];
    if (first.length <= left) {
      parts.push(first);
      conn.queue.shift();
      left -= first.length;
    } else {
      parts.push(first.subarray(0, left));
      conn.queue[0] = first.subarray(left);
      left = 0;
    }
  }
  conn.queued -= n;
  return parts.length === 1 ? parts[0] : Buffer.concat(parts, n);
}

// Everything queued, as one buffer, left in the queue.
function peek(conn) {
  if (conn.queue.length > 1) conn.queue = [Buffer.concat(conn.queue, conn.queued)];
  return conn.queue[0] ?? Buffer.alloc(0);
}

function pump(conn) {
  if (conn.reader !== null) conn.reader.consume();
  const hungry = conn.reader !== null && conn.reader.hungry();
  if (!conn.holding && (hungry || conn.queued < READ_AHEAD)) conn.socket.resume();
  else conn.socket.pause();
}

// A head: everything up to and including the empty line that ends it.
// Empty lines before a request line are dropped (RFC 9112 section 2.2).
function headReader(conn, maxBytes, report) {
  return {
    hungry: () => true,
    consume() {
      while (conn.queued >= 2 && peek(conn).subarray(0, 2).equals(CRLF)) take(conn, 2);
      const view = peek(conn).subarray(0, maxBytes);
      const end = view.indexOf(END_OF_HEAD);
      if (end !== -1) {
        conn.reader = null;
        report('head', { head: take(conn, end + 4).toString('latin1') });
      } else if (conn.queued >= maxBytes) {
        conn.reader = null;
        report('overflow', {});
      } else if (conn.ended) {
        conn.reader = null;
      }
    },
    fail() {
      conn.reader = null;
    },
  };
}

// A request body as a stream, read as Elm said: `length` bytes, or chunked
// under the limits Elm gave. Destroying the stream (an abort does) lets go
// of the socket at once.
function bodyStream(conn, framed) {
  const decoder = framed.kind === 'chunked' ? new ChunkedDecoder(framed) : null;
  let remaining = framed.kind === 'length' ? framed.length : 0;
  let finished = decoder === null && remaining === 0;
  let wanting = false;
  let reader = null;
  const stream = new Readable({
    highWaterMark: 64 * 1024,
    read() {
      wanting = true;
      pump(conn);
    },
    destroy(err, done) {
      if (conn.reader === reader) conn.reader = null;
      pump(conn);
      done(err);
    },
  });
  reader = {
    hungry: () => wanting && !finished,
    consume() {
      try {
        while (wanting && !finished && conn.queued > 0) {
          if (decoder !== null) {
            const out = decoder.push(peek(conn));
            take(conn, out.used);
            for (const piece of out.data) wanting = stream.push(piece);
            finished = out.done;
          } else {
            const piece = take(conn, Math.min(remaining, conn.queued));
            remaining -= piece.length;
            finished = remaining === 0;
            wanting = stream.push(piece);
          }
        }
      } catch (err) {
        conn.reader = null;
        stream.destroy(err);
        return;
      }
      if (finished) {
        conn.reader = null;
        stream.push(null);
      } else if (conn.ended && conn.queued === 0) {
        conn.reader = null;
        stream.destroy(fault('Error', 'ERR_STREAM_PREMATURE_CLOSE', 'the client ended its side before the body did'));
      }
    },
    fail(err) {
      conn.reader = null;
      stream.destroy(err);
    },
  };
  if (finished) stream.push(null);
  else conn.reader = reader;
  return stream;
}

// Writes to a socket with its backpressure, and never ends it: what happens
// to the connection after a download is Elm's to say.
function socketSink(socket) {
  let pending = null;
  const settle = (err) => {
    if (pending === null) return;
    const { done, onDrain, onClose } = pending;
    pending = null;
    socket.off('drain', onDrain);
    socket.off('close', onClose);
    done(err);
  };
  return new Writable({
    highWaterMark: 64 * 1024,
    write(chunk, _encoding, done) {
      if (socket.destroyed) return done(fault('Error', 'ECONNRESET', 'the client went away'));
      if (socket.write(chunk)) return done();
      const onDrain = () => settle();
      const onClose = () => settle(fault('Error', 'ECONNRESET', 'the client went away'));
      pending = { done, onDrain, onClose };
      socket.once('drain', onDrain);
      socket.once('close', onClose);
      return undefined;
    },
    destroy(err, done) {
      settle(err ?? fault('AbortError', 'ABORT_ERR', 'This operation was aborted'));
      done(err);
    },
  });
}

// ---------------------------------------------------------------------------
// The drivers: one host primitive each.

function createDrivers({ handles, conns, knobs }) {
  // Fault injection for tests: a slow device. Deliberately not abortable,
  // because a slow syscall is not abortable either.
  const disk = (run) => async (op, ctx) => {
    const range = knobs.diskLatencyMs;
    if (range) await sleep(range[0] + Math.random() * (range[1] - range[0]));
    return run(op, ctx);
  };

  const handle = (name) => {
    const fh = handles.get(name);
    if (fh === undefined) throw fault('ProtocolError', null, `no open handle named ${name}`);
    return fh;
  };

  const connection = (id) => {
    const conn = conns.get(id);
    if (conn === undefined || conn.socket.destroyed) throw fault('Error', 'ECONNRESET', 'the client disconnected before this step ran');
    return conn;
  };

  const flags = (names) => names.reduce((acc, name) => {
    if (typeof fsConstants[name] !== 'number') throw fault('ProtocolError', null, `unknown open flag ${name}`);
    return acc | fsConstants[name];
  }, 0);

  // Counts bytes, enforces Elm's limit, hashes if asked, and reports the
  // count; the report is stamped with both clocks as it goes out. Reports are
  // throttled to one per PROGRESS_EVERY_MS, and a throttled chunk schedules a
  // trailing report, so no byte goes unreported for longer than that. The
  // first report (0 bytes) starts Elm's meter.
  const meter = (ctx, limit, hashing) => {
    let bytes = 0;
    let last = 0;
    let trailing = null;
    const digest = hashing ? createHash('sha256') : null;
    const report = (running) => {
      last = performance.now();
      ctx.progress(bytes, running);
    };
    report(true);
    const tap = new Transform({
      transform(chunk, _encoding, done) {
        bytes += chunk.length;
        if (limit !== null && bytes > limit) return done(fault('LimitExceeded', 'ACKBERRY_LIMIT', `more than ${limit} bytes`));
        digest?.update(chunk);
        const wait = last + PROGRESS_EVERY_MS - performance.now();
        if (wait <= 0) report(true);
        else if (trailing === null) {
          trailing = setTimeout(() => {
            trailing = null;
            report(true);
          }, wait);
        }
        return done(null, chunk);
      },
    });
    return {
      tap,
      bytes: () => bytes,
      sha256: () => digest.digest('hex'),
      stop: () => {
        clearTimeout(trailing);
        trailing = null;
        report(false);
      },
    };
  };

  // Elm's invitation, if it sent one (100 Continue), then the body.
  const body = (op) => {
    const conn = connection(op.conn);
    if (op.invite !== null) conn.socket.write(Buffer.from(op.invite, 'latin1'));
    return bodyStream(conn, op.framing);
  };

  function httpGet(op, ctx) {
    return new Promise((resolve, reject) => {
      const req = http.get(op.url, { signal: ctx.signal, agent: false }, (res) => {
        const chunks = [];
        let bytes = 0;
        res.on('data', (chunk) => {
          bytes += chunk.length;
          if (bytes <= op.maxBytes) chunks.push(chunk);
        });
        res.on('end', () => {
          let text;
          if (bytes > op.maxBytes) text = { tag: 'tooLarge', limit: op.maxBytes };
          else {
            try {
              text = { tag: 'text', text: utf8.decode(Buffer.concat(chunks)) };
            } catch {
              text = { tag: 'notUtf8' };
            }
          }
          resolve({ status: res.statusCode, body: text });
        });
        res.on('error', reject);
        res.on('close', () => reject(res.errored ?? new Error('upstream response closed before its end')));
      });
      req.on('error', reject);
    });
  }

  return {
    open: disk(async (op, ctx) => {
      if (handles.has(op.handle)) throw fault('ProtocolError', null, `handle ${op.handle} is already open`);
      handles.set(op.handle, await fs.open(op.path, flags(op.flags)));
      ctx.opened(op.handle);
      return {};
    }),
    write: disk(async (op) => {
      const bytes = Buffer.from(op.text, 'utf8');
      await handle(op.handle).writeFile(bytes); // loops until every byte is written
      return { bytes: bytes.length, sha256: createHash('sha256').update(bytes).digest('hex') };
    }),
    sync: disk(async (op) => {
      await handle(op.handle).sync();
      return {};
    }),
    close: disk(async (op) => {
      const fh = handle(op.handle);
      handles.delete(op.handle); // Linux frees the fd even when close(2) fails
      await fh.close();
      return {};
    }),
    stat: disk(async (op) => {
      const stat = await handle(op.handle).stat();
      return { size: stat.size, isFile: stat.isFile() };
    }),
    link: disk(async (op) => {
      await fs.link(op.from, op.to);
      return {};
    }),
    unlink: disk(async (op) => {
      await fs.unlink(op.path);
      return {};
    }),
    syncDir: disk(async (op) => {
      await syncDirectory(op.path);
      return {};
    }),
    // mkdir -p, durably: every directory it creates has its entry fsync'd
    // in its parent. An existing directory is success; an existing anything
    // else is EEXIST, a file in the way is ENOTDIR.
    ensureDir: disk(async (op) => {
      const created = await fs.mkdir(op.path, { recursive: true });
      if (created === undefined) return { created: null };
      const top = path.resolve(created);
      for (let dir = path.resolve(op.path); ; dir = path.dirname(dir)) {
        await syncDirectory(path.dirname(dir));
        if (dir === top || path.dirname(dir) === dir) break;
      }
      return { created: top };
    }),
    // Every entry with its own lstat outcome, nothing filtered, and the host
    // clock read before the directory was: an entry written during the
    // listing has an mtime after `at`, never before.
    listDir: disk(async (op) => {
      const at = Date.now();
      const entries = [];
      for (const name of await fs.readdir(op.path)) {
        try {
          const stat = await fs.lstat(path.join(op.path, name));
          entries.push({ name, stat: { ok: { isFile: stat.isFile(), mtimeMs: stat.mtimeMs } } });
        } catch (err) {
          entries.push({ name, stat: { error: hostError(err) } });
        }
      }
      return { at, entries };
    }),
    // One unlink per path, each with its own outcome; a failure does not
    // stop the rest.
    unlinkEach: disk(async (op) => {
      const outcomes = [];
      for (const target of op.paths) {
        try {
          await fs.unlink(target);
          outcomes.push({ ok: {} });
        } catch (err) {
          outcomes.push({ error: hostError(err) });
        }
      }
      return { outcomes };
    }),
    httpGet,
    // The request body, read into memory: only for small bodies Elm must see.
    readBody: async (op, ctx) => {
      const source = body(op);
      const m = meter(ctx, op.maxBytes, false);
      const chunks = [];
      try {
        const sink = new Writable({ write(chunk, _encoding, done) { chunks.push(chunk); done(); } });
        await pipeline(source, m.tap, sink, { signal: ctx.signal });
      } finally {
        m.stop();
      }
      try {
        return { tag: 'text', text: utf8.decode(Buffer.concat(chunks)) };
      } catch {
        return { tag: 'notUtf8' };
      }
    },
    // The request body, socket straight to disk. Nothing is buffered beyond
    // the streams' high-water marks; an abort destroys the body stream,
    // which lets go of the socket, and the fd stays open for the cleanup.
    pipeBody: async (op, ctx) => {
      const source = body(op);
      const fh = handle(op.handle);
      const m = meter(ctx, op.maxBytes, true);
      try {
        await pipeline(source, m.tap, fileSink(fh), { signal: ctx.signal });
      } finally {
        m.stop();
      }
      return { bytes: m.bytes(), sha256: m.sha256() };
    },
    // A file as the response body: Elm's head, then exactly `length` bytes.
    // The fd stays open for the transaction's cleanup.
    stream: async (op, ctx) => {
      const conn = connection(op.conn);
      const fh = handle(op.handle);
      conn.socket.write(Buffer.from(op.head, 'latin1'));
      const m = meter(ctx, null, false);
      try {
        await pipeline(fileSource(fh, op.length), m.tap, socketSink(conn.socket), { signal: ctx.signal });
      } finally {
        m.stop();
      }
      return { bytes: m.bytes() };
    },
  };
}

// ---------------------------------------------------------------------------

function start({
  port,
  dataDir,
  upstream,
  deadlineMs = 2000,
  transferDeadlineMs = 600000,
  transferRate = { bytes: 8192, perMs: 2000 },
  deliveryRate = { bytes: 8192, perMs: 2000 },
  jsonLimit = 1024 * 1024,
  uploadLimit = 4 * 1024 ** 3,
  staleAfterMs = 2 * 3600 * 1000,
  sweepEveryMs = 15 * 60 * 1000,
  head = { maxHeadBytes: 16 * 1024, maxTarget: 8 * 1024, maxFields: 100 },
  headDeadlineMs = 10000,
  idleMs = 5000,
  tickMs = 100,
  linger = { deadlineMs: 500, budgetBytes: 128 * 1024, maxSockets: 64 },
  chunked = { maxLine: 8 * 1024, maxTrailer: 16 * 1024 },
  diskLatencyMs = null,
  log = console.error,
}) {
  const app = Elm.Main.init({
    flags: {
      dataDir,
      upstream,
      deadlineMs,
      transferDeadlineMs,
      transferRate,
      deliveryRate,
      jsonLimit,
      uploadLimit,
      progressEveryMs: PROGRESS_EVERY_MS,
      staleAfterMs,
      sweepEveryMs,
      head,
      headDeadlineMs,
      idleMs,
      tickMs,
      linger,
      chunked,
    },
  });
  const conns = new Map(); // connection id -> { socket, queue, reader, ... }
  const handles = new Map(); // handle name -> fs.FileHandle
  const events = new EventEmitter(); // observation only; nothing here reads it
  const knobs = { diskLatencyMs };

  // Every event Elm hears carries both clocks, read as it goes out.
  const tell = (event, fields) => app.ports.fromBridge.send({ event, ...fields, mono: performance.now(), wall: Date.now() });

  const reclaim = async (opened) => {
    const leaked = [];
    for (const name of opened) {
      const fh = handles.get(name);
      if (fh === undefined) continue;
      handles.delete(name);
      try {
        await fh.close();
        leaked.push(name);
      } catch (err) {
        leaked.push(`${name} (close failed: ${err.code})`);
      }
    }
    return leaked;
  };

  const vm = createInterpreter({
    drivers: createDrivers({ handles, conns, knobs }),
    report: (tx, report) => tell('report', { tx, report }),
    progress: (tx, bytes, running) => tell('progress', { tx, bytes, running }),
    reclaim,
    events,
  });

  const accept = (socket) => {
    const id = randomUUID();
    const conn = { id, socket, queue: [], queued: 0, reader: null, ended: false, closing: null, holding: false, discarded: 0, left: false };
    const leaving = () => {
      if (conn.left) return;
      conn.left = true;
      events.emit('disconnect', { conn: id, at: now() });
    };
    conns.set(id, conn);
    socket.setNoDelay(true);
    socket.on('data', (chunk) => {
      conn.queue.push(chunk);
      conn.queued += chunk.length;
      pump(conn);
    });
    socket.on('end', () => {
      conn.ended = true;
      if (conn.closing === null) {
        leaving();
        tell('ended', { conn: id });
      }
      pump(conn);
    });
    socket.on('error', () => {}); // the close that follows says what happened
    socket.on('close', () => {
      conns.delete(id);
      if (conn.reader !== null) conn.reader.fail(fault('Error', 'ECONNRESET', 'the client went away'));
      const how = conn.closing ?? 'peer';
      if (how === 'peer') leaving();
      events.emit('released', { conn: id, how, discarded: conn.discarded, at: now() });
      tell('closed', { conn: id, how, discarded: conn.discarded });
    });
    events.emit('accepted', { conn: id, at: now() });
    tell('accepted', { conn: id });
    pump(conn);
  };

  const close = (conn) => {
    conn.closing = 'asked';
    conn.socket.end();
    if (conn.socket.writableFinished) conn.socket.destroy();
    else conn.socket.once('finish', () => conn.socket.destroy());
  };

  // The staged close of RFC 9112 section 9.6: FIN after the response, then
  // the client's bytes are read and dropped, never parsed, until it hangs up
  // or one of Elm's bounds is crossed. At the budget, cut closes at once and
  // hold stops reading until the deadline.
  const dissipate = (conn, { deadlineMs: ms, budgetBytes, whenSpent }) => {
    conn.closing = 'hungUp';
    const cut = (how) => {
      if (conn.socket.destroyed) return;
      conn.closing = how;
      conn.socket.destroy();
    };
    let spent = false;
    const deadline = setTimeout(() => cut(spent ? 'budget' : 'deadline'), ms);
    conn.socket.once('close', () => clearTimeout(deadline));
    conn.reader = {
      hungry: () => !spent,
      consume() {
        if (!spent && conn.queued > 0) {
          conn.discarded += conn.queued;
          take(conn, conn.queued);
          if (conn.discarded >= budgetBytes) {
            spent = true;
            if (whenSpent === 'cut') cut('budget');
            else {
              conn.closing = 'budget';
              conn.holding = true;
            }
          }
        }
        if (conn.ended && !spent) cut('hungUp');
      },
      fail() {
        conn.reader = null;
      },
    };
    conn.socket.end();
    pump(conn);
  };

  let alarm = null;

  const server = net.createServer({ allowHalfOpen: true, pauseOnConnect: true }, accept);

  app.ports.toBridge.subscribe((instructions) => {
    for (const instruction of instructions) {
      const conn = instruction.conn === undefined ? undefined : conns.get(instruction.conn);
      switch (instruction.do) {
        case 'awaitHead':
          if (conn !== undefined && conn.closing === null) {
            conn.reader = headReader(conn, instruction.maxBytes, (event, fields) => tell(event, { conn: conn.id, ...fields }));
            pump(conn);
          }
          break;
        case 'send':
          if (conn !== undefined && !conn.socket.destroyed) conn.socket.write(Buffer.from(instruction.text, 'utf8'));
          break;
        case 'close':
          if (conn !== undefined && conn.closing === null) close(conn);
          break;
        case 'dissipate':
          if (conn !== undefined && conn.closing === null) dissipate(conn, instruction);
          break;
        case 'begin': vm.begin(instruction); break;
        case 'extend': vm.extend(instruction); break;
        case 'abort': vm.abort(instruction); break;
        case 'alarm':
          clearTimeout(alarm);
          alarm = instruction.inMs === null ? null : setTimeout(() => {
            alarm = null;
            tell('alarm', {});
          }, instruction.inMs);
          break;
        case 'note':
          events.emit('request', { request: instruction.request, conn: instruction.conn, method: instruction.method, target: instruction.target });
          break;
        case 'log':
          events.emit('fault', instruction.fault);
          log(JSON.stringify(instruction.fault));
          break;
        case 'listen': server.listen(port); break;
        case 'halt':
          events.emit('halt', instruction.reason);
          log(JSON.stringify({ event: 'halt', reason: instruction.reason }));
          break;
        default:
          throw new Error(`unknown instruction ${JSON.stringify(instruction.do)}: Elm and the bridge disagree on the protocol`);
      }
    }
  });

  // Stop accepting, and tell Elm, which closes connections waiting for a
  // head and lets exchanges in flight finish.
  const stop = () => {
    server.close();
    tell('serverClosed', {});
  };

  return { server, stop, conns, handles, txs: vm.txs, events, knobs };
}

module.exports = { start, createInterpreter, hostError, ChunkedDecoder };

if (require.main === module) {
  const dataDir = path.resolve(process.env.DATA_DIR ?? 'data');
  const { server, events } = start({
    port: Number(process.env.PORT ?? 3000),
    dataDir,
    upstream: process.env.UPSTREAM ?? 'http://127.0.0.1:3001',
  });
  server.on('listening', () => console.log(`listening on http://localhost:${server.address().port}, data in ${dataDir}`));
  // Nothing listens and no timer runs after a halt, so the process exits.
  events.on('halt', () => { process.exitCode = 1; });
}
