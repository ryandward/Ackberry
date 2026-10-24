'use strict';

// The bridge: an HTTP front end, a table of host resources, and an
// interpreter for the transaction algebra defined in src/Tx.elm.
//
// It decides nothing. Elm sends one ordered array of instructions per update
// and the bridge applies them in order:
//
//   respond  write an inline response
//   begin    start a transaction: an ordered list of steps
//   extend   append steps to a paused transaction
//   abort    preempt a transaction
//   log      write a fault line
//   listen   start accepting connections (Elm sends it once the data
//            directory's layout exists)
//   halt     Elm refuses to serve; say why
//
// The interpreter knows sequencing, a cleanup stack and a commit flag; it
// does not know what any operation means. The drivers know how to perform
// one operation each and how to sever it when its AbortSignal fires; they do
// not know what a transaction is. tests/vm checks the interpreter against
// Elm's reference semantics on random programs.

const http = require('node:http');
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

// ---------------------------------------------------------------------------
// The drivers: one host primitive each.

// fsync on a directory: makes its entries, not the files they name, durable.
async function syncDirectory(dirPath) {
  const dir = await fs.open(dirPath, 'r');
  try {
    await dir.sync();
  } finally {
    await dir.close();
  }
}

function createDrivers({ handles, exchanges, knobs }) {
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

  const exchange = (ctx) => {
    const ex = exchanges.get(ctx.request);
    if (ex === undefined) throw fault('Error', 'ECONNRESET', 'the client disconnected before this step ran');
    return ex;
  };

  const flags = (names) => names.reduce((acc, name) => {
    if (typeof fsConstants[name] !== 'number') throw fault('ProtocolError', null, `unknown open flag ${name}`);
    return acc | fsConstants[name];
  }, 0);

  // Counts bytes, enforces Elm's limit, hashes if asked, and reports the
  // count with the time it was taken. Reports are throttled to one per
  // PROGRESS_EVERY_MS, and a throttled chunk schedules a trailing report, so
  // no byte goes unreported for longer than that. The first report (0 bytes)
  // starts Elm's meter at the bridge's clock.
  const meter = (ctx, limit, hashing) => {
    let bytes = 0;
    let last = 0;
    let trailing = null;
    const digest = hashing ? createHash('sha256') : null;
    const report = (running) => {
      last = Date.now();
      ctx.progress(bytes, running, last);
    };
    report(true);
    const tap = new Transform({
      transform(chunk, _encoding, done) {
        bytes += chunk.length;
        if (limit !== null && bytes > limit) return done(fault('LimitExceeded', 'ACKBERRY_LIMIT', `more than ${limit} bytes`));
        digest?.update(chunk);
        const wait = last + PROGRESS_EVERY_MS - Date.now();
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
          let body;
          if (bytes > op.maxBytes) body = { tag: 'tooLarge', limit: op.maxBytes };
          else {
            try {
              body = { tag: 'text', text: utf8.decode(Buffer.concat(chunks)) };
            } catch {
              body = { tag: 'notUtf8' };
            }
          }
          resolve({ status: res.statusCode, body });
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
      const { req, res } = exchange(ctx);
      if (op.sendContinue) res.writeContinue();
      const m = meter(ctx, op.maxBytes, false);
      const chunks = [];
      try {
        const sink = new Writable({ write(chunk, _encoding, done) { chunks.push(chunk); done(); } });
        await pipeline(req, m.tap, sink, { signal: ctx.signal });
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
    // the streams' high-water marks; aborting destroys the socket and the
    // write stream, and the fd stays open for the transaction's cleanup.
    pipeBody: async (op, ctx) => {
      const { req, res } = exchange(ctx);
      const fh = handle(op.handle);
      if (op.sendContinue) res.writeContinue();
      const m = meter(ctx, op.maxBytes, true);
      try {
        await pipeline(req, m.tap, fileSink(fh), { signal: ctx.signal });
      } finally {
        m.stop();
      }
      return { bytes: m.bytes(), sha256: m.sha256() };
    },
    // A file as the response body. This step answers the client, so it
    // takes the exchange; the fd stays open for the transaction's cleanup.
    stream: async (op, ctx) => {
      const { res } = exchange(ctx);
      exchanges.delete(ctx.request);
      const fh = handle(op.handle);
      res.writeHead(op.status, op.headers);
      const m = meter(ctx, null, false);
      try {
        await pipeline(fileSource(fh, op.length), m.tap, res, { signal: ctx.signal });
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
    },
  });
  const exchanges = new Map(); // requestId -> { req, res }; the body is unread until a step reads it
  const handles = new Map(); // handle name -> fs.FileHandle
  const events = new EventEmitter(); // observation only; nothing here reads it
  const knobs = { diskLatencyMs };

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
    drivers: createDrivers({ handles, exchanges, knobs }),
    report: (request, report) => app.ports.txReported.send([request, report]),
    progress: (request, bytes, running, at) => app.ports.txProgress.send({ request, bytes, running, at }),
    reclaim,
    events,
  });

  // A miss is legitimate: the client can leave while the instruction sits in
  // Elm's port mailbox. Ids are never reused, so nothing else can be hit.
  const respond = ({ request, status, headers, body }) => {
    const ex = exchanges.get(request);
    if (ex === undefined) return;
    exchanges.delete(request);
    ex.res.writeHead(status, headers).end(body);
  };

  const onRequest = (req, res) => {
    const id = randomUUID();
    exchanges.set(id, { req, res });
    // Not req 'close': since Node 16 it fires once the body has been read.
    // res 'close' fires when the response finished or the connection died;
    // only a death leaves the id in the map.
    res.on('close', () => {
      if (exchanges.delete(id)) {
        events.emit('disconnect', { request: id, at: now() });
        app.ports.clientDisconnected.send(id);
      }
    });
    events.emit('request', { request: id, method: req.method, target: req.url });
    app.ports.incomingRequest.send([id, Date.now(), { method: req.method, target: req.url, headers: req.rawHeaders }]);
  };

  const server = http.createServer(onRequest);
  // Without this, Node answers "100 Continue" on Elm's behalf and the client
  // starts sending a body Elm may be about to refuse.
  server.on('checkContinue', onRequest);
  // Elm's janitor keeps a timer while the server serves; this lets it go.
  server.on('close', () => app.ports.serverClosed.send(null));

  // Elm's init instructions arrive after this subscription, never before:
  // its outgoing port manager starts with a zero-delay sleep.
  app.ports.toBridge.subscribe((instructions) => {
    for (const instruction of instructions) {
      switch (instruction.do) {
        case 'respond': respond(instruction); break;
        case 'begin': vm.begin(instruction); break;
        case 'extend': vm.extend(instruction); break;
        case 'abort': vm.abort(instruction); break;
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
  return { server, exchanges, handles, txs: vm.txs, events, knobs };
}

module.exports = { start, createInterpreter, hostError };

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
