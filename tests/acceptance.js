'use strict';

// End-to-end acceptance matrix against the real bridge, the compiled Elm
// worker, a real filesystem and a real (mock) upstream on another port.
// Exits non-zero on the first failed check, or if anything keeps the process
// alive after everything closes.

const http = require('node:http');
const net = require('node:net');
const fs = require('node:fs');
const fsp = require('node:fs/promises');
const path = require('node:path');
const assert = require('node:assert/strict');
const { execFileSync } = require('node:child_process');
const { createHash, randomBytes } = require('node:crypto');
const { Readable } = require('node:stream');
const autocannon = require('autocannon');
const { start } = require('../server.js');
const { startUpstream } = require('../upstream.js');

const MiB = 1024 * 1024;
const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const openFds = () => fs.readdirSync('/proc/self/fd').length;
const listening = (server) => new Promise((resolve) => server.on('listening', resolve));
const ms = (ns) => Number(ns) / 1e6;
const quantile = (xs, q) => [...xs].sort((a, b) => a - b)[Math.min(xs.length - 1, Math.floor(q * xs.length))];

async function waitFor(what, predicate, timeoutMs = 5000) {
  const deadline = Date.now() + timeoutMs;
  while (!(await predicate())) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await sleep(10);
  }
}

function send(port, { method = 'GET', path: target, body, headers = {} }) {
  return new Promise((resolve, reject) => {
    const all = body === undefined ? headers : { 'content-length': Buffer.byteLength(body), ...headers };
    const req = http.request({ port, method, path: target, headers: all, agent: false }, (res) => {
      const chunks = [];
      res.on('data', (c) => chunks.push(c));
      res.on('end', () => {
        const raw = Buffer.concat(chunks);
        resolve({ status: res.statusCode, headers: res.headers, raw, text: raw.toString('utf8') });
      });
      res.on('error', reject);
    });
    req.on('error', reject);
    // A hung request is the failure this suite exists to catch; name it.
    req.setTimeout(10000, () => req.destroy(new Error(`no response within 10 s: ${method} ${target}`)));
    if (body !== undefined) req.write(body);
    req.end();
  });
}

// A raw socket, for clients that misbehave on purpose: what they send, when,
// and when they vanish.
function rawClient(port, head) {
  const sock = net.connect(port, '127.0.0.1');
  const received = [];
  sock.on('data', (c) => received.push(c));
  sock.on('error', () => {});
  const closed = new Promise((resolve) => sock.on('close', resolve));
  sock.write(head);
  return { sock, closed, text: () => Buffer.concat(received).toString('latin1') };
}

function sendAndCut(port, { method = 'POST', path: target, body, cut }) {
  return new Promise((resolve) => {
    const req = http.request({ port, method, path: target, headers: { 'content-length': Buffer.byteLength(body) }, agent: false });
    req.on('error', () => {});
    req.on('response', (res) => res.resume());
    req.on('close', resolve);
    req.write(body);
    req.end();
    cut(req);
  });
}

const json = (r) => JSON.parse(r.text);
let checks = 0;
async function check(name, fn) {
  await fn();
  checks += 1;
  console.log(`ok  ${name}`);
}

async function main() {
  // Not os.tmpdir(): /tmp is often tmpfs, where fsync is a no-op.
  const root = process.env.ACKBERRY_TEST_DIR ?? path.join(__dirname, '..', '.test-data');
  await fsp.mkdir(root, { recursive: true });
  const dataDir = await fsp.mkdtemp(path.join(root, 'run-'));
  console.log(`data dir ${dataDir} on ${fs.statfsSync(dataDir).type === 0x01021994 ? 'tmpfs (fsync is a no-op!)' : 'a disk-backed filesystem'}`);

  const upstream = startUpstream({ port: 0 });
  await listening(upstream.server);
  const upstreamUrl = `http://127.0.0.1:${upstream.server.address().port}`;
  const app = start({
    port: 0,
    dataDir,
    upstream: upstreamUrl,
    transferRate: { bytes: 4096, perMs: 1000 },
    deliveryRate: { bytes: 4096, perMs: 1000 },
    uploadLimit: 256 * MiB,
    log: () => {},
  });
  await listening(app.server);
  const port = app.server.address().port;
  const get = (target) => send(port, { path: target });
  const post = (target, body) => send(port, { method: 'POST', path: target, body });
  const stats = async () => json(await get('/stats'));
  const files = () => fs.readdirSync(dataDir);
  const tmpFiles = () => files().filter((f) => f.endsWith('.tmp'));

  const faults = [];
  const trail = [];
  const idOf = new Map(); // target -> request id
  app.events.on('fault', (f) => faults.push(f));
  app.events.on('request', (r) => idOf.set(r.target, r.request));
  app.events.on('io', (e) => trail.push({ name: `${e.kind}:${e.phase}`, request: e.request, at: e.at }));
  app.events.on('tx', (e) => trail.push({ name: `tx:${e.phase}`, request: e.request, at: e.at }));
  app.events.on('disconnect', (d) => trail.push({ name: 'disconnect', request: d.request, at: d.at }));
  const of = (id) => trail.filter((e) => e.request === id);
  const first = (id, name) => of(id).find((e) => e.name === name);

  const quiescent = (timeoutMs = 5000) =>
    waitFor('quiescence', async () => {
      const s = await stats();
      return s.inFlight === 0 && s.draining === 0 && app.txs.size === 0 && app.handles.size === 0 && app.exchanges.size === 0;
    }, timeoutMs);

  const blob = randomBytes(10 * MiB);
  fs.writeFileSync(path.join(dataDir, 'blob_1.bin'), blob);
  fs.writeFileSync(path.join(dataDir, 'blob_3.bin'), '');
  fs.mkdirSync(path.join(dataDir, 'blob_4.bin'));
  execFileSync('mkfifo', [path.join(dataDir, 'blob_7.bin')]);
  fs.symlinkSync('blob_9.bin', path.join(dataDir, 'blob_9.bin'));
  const baselineFds = openFds();

  // PHASE 1 ----------------------------------------------------------------

  await check('phase 1: /health, /items/123, /items/abc, /echo exact', async () => {
    assert.deepEqual(json(await get('/health')), { status: 'ok' });
    assert.equal(json(await get('/items/123')).id, 123);
    const bad = await get('/items/abc');
    assert.deepEqual([bad.status, json(bad).segment], [400, 'abc']);
    const body = '{"hello":"world","n":12345678901234567890,"k":1,"k":2}';
    const echo = await send(port, { method: 'POST', path: '/echo', body, headers: { 'content-type': 'application/x-www-form-urlencoded' } });
    assert.deepEqual([echo.status, echo.text], [200, body]);
  });

  await check('phase 1: 20 concurrent /delayed/42 over real HTTP; 300 mixed ids, each to its own socket', async () => {
    for (const r of await Promise.all(Array.from({ length: 20 }, () => get('/delayed/42')))) assert.deepEqual([r.status, json(r).id], [200, 42]);
    const ids = Array.from({ length: 300 }, (_, i) => [1, 7, 42, 123, 5, 999][i % 6]);
    const rs = await Promise.all(ids.map((n) => get(`/delayed/${n}`)));
    rs.forEach((r, i) => {
      const known = [1, 7, 42, 123].includes(ids[i]);
      assert.equal(r.status, known ? 200 : 404);
      assert.equal(known ? json(r).id : json(r).itemId, ids[i]);
    });
  });

  await check('upstream: junk, wrong item, 500, reset mid-body, oversized -> 502; hung -> 504 and its socket is severed', async () => {
    for (const id of [13, 14, 500, 777, 888]) assert.equal((await get(`/delayed/${id}`)).status, 502, `id ${id}`);
    assert.equal((await get('/delayed/666')).status, 504);
    await waitFor('the upstream socket to close', () => upstream.sockets.size === 0, 1000);
  });

  // PHASE 2: BUFFERED INGEST ---------------------------------------------

  await check('ingest: 201 with the sha256 of the exact bytes; .final holds them; a second ingest is 409', async () => {
    const body = '{"hello":"world","n":12345678901234567890}';
    const r = await post('/ingest/42', body);
    assert.deepEqual(json(r), { itemId: 42, bytes: Buffer.byteLength(body), sha256: sha256(body) });
    assert.equal(fs.readFileSync(path.join(dataDir, 'file_42.final'), 'utf8'), body);
    assert.equal((await post('/ingest/42', '{"again":1}')).status, 409);
    assert.deepEqual(tmpFiles(), []);
  });

  await check('POISON DISK: chmod 000 -> ingest 500 {EACCES, open}, blob 500 not 404, nothing pending', async () => {
    fs.chmodSync(dataDir, 0o000);
    try {
      const r = await post('/ingest/43', '{"a":1}');
      assert.deepEqual([r.status, json(r).hostError], [500, { code: 'EACCES', syscall: 'open' }]);
      assert.doesNotMatch(r.text, new RegExp(dataDir), 'server paths leaked to the client');
      const b = await get('/blobs/1');
      assert.deepEqual([b.status, json(b).hostError], [500, { code: 'EACCES', syscall: 'open' }]);
    } finally {
      fs.chmodSync(dataDir, 0o755);
    }
    await quiescent();
  });

  await check('compensation can itself fail: a denied rollback reports its orphan', async () => {
    const target = '/ingest/44';
    const onLink = (e) => {
      if (e.phase === 'start' && e.kind === 'link' && e.request === idOf.get(target)) fs.chmodSync(dataDir, 0o000);
    };
    app.events.on('io', onLink);
    let r;
    try {
      r = await post(target, '{"a":1}');
      await quiescent();
    } finally {
      app.events.off('io', onLink);
      fs.chmodSync(dataDir, 0o755);
    }
    assert.deepEqual([r.status, json(r).hostError], [500, { code: 'EACCES', syscall: 'link' }]);
    const orphan = faults.find((f) => f.event === 'orphan');
    assert.equal(orphan?.path, path.join(dataDir, 'file_44.tmp'));
    assert.deepEqual(tmpFiles(), ['file_44.tmp'], 'the reported orphan is the only temp file');
    fs.unlinkSync(path.join(dataDir, 'file_44.tmp'));
  });

  await check('MID-FLIGHT CUTOFF (buffered): socket killed during the write; it lands, then unlink, then close', async () => {
    app.knobs.diskLatencyMs = [40, 40];
    const target = '/ingest/99';
    try {
      await sendAndCut(port, {
        path: target,
        body: JSON.stringify({ blob: 'x'.repeat(512 * 1024) }),
        cut: (req) => {
          const onWrite = (e) => {
            if (e.phase === 'start' && e.kind === 'write' && e.request === idOf.get(target)) {
              app.events.off('io', onWrite);
              req.destroy();
            }
          };
          app.events.on('io', onWrite);
        },
      });
      await quiescent();
    } finally {
      app.knobs.diskLatencyMs = null;
    }
    assert.deepEqual(of(idOf.get(target)).map((e) => e.name), [
      'readBody:start', 'readBody:end', 'open:start', 'open:end',
      'write:start', 'disconnect', 'tx:abort', 'write:end',
      'unlink:start', 'unlink:end', 'close:start', 'close:end', 'tx:settled',
    ]);
    assert.ok(!files().includes('file_99.tmp') && !files().includes('file_99.final'));
  });

  await check('OUT-OF-ORDER CONCURRENCY (buffered): 100 commits + 20 lease contenders + 20 aborts at 10-150 ms disk latency', async () => {
    app.knobs.diskLatencyMs = [10, 150];
    const bodies = new Map();
    const body = (id, who) => {
      const k = `${id}/${who}`;
      if (!bodies.has(k)) bodies.set(k, JSON.stringify({ id, who, pad: randomBytes(2048).toString('hex') }));
      return bodies.get(k);
    };
    const plain = Array.from({ length: 100 }, (_, i) => ({ id: 2000 + i, who: 'only' }));
    const contested = Array.from({ length: 10 }, (_, i) => [{ id: 3000 + i, who: 'a' }, { id: 3000 + i, who: 'b' }]).flat();
    const doomed = Array.from({ length: 20 }, (_, i) => ({ id: 4000 + i, who: 'doomed' }));
    let results;
    try {
      results = await Promise.all([
        ...[...plain, ...contested].map((r) => post(`/ingest/${r.id}`, body(r.id, r.who)).then((res) => ({ ...r, res }))),
        ...doomed.map((r) => sendAndCut(port, { path: `/ingest/${r.id}`, body: body(r.id, r.who), cut: (req) => setTimeout(() => req.destroy(), Math.random() * 400) }).then(() => ({ ...r, res: null }))),
      ]);
      await quiescent();
    } finally {
      app.knobs.diskLatencyMs = null;
    }
    for (const r of results.filter((x) => x.who === 'only')) {
      assert.equal(json(r.res).sha256, sha256(body(r.id, r.who)));
      assert.equal(fs.readFileSync(path.join(dataDir, `file_${r.id}.final`), 'utf8'), body(r.id, r.who));
    }
    for (let i = 0; i < 10; i += 1) {
      const pair = results.filter((x) => x.id === 3000 + i);
      assert.deepEqual(pair.map((x) => x.res.status).sort(), [201, 409]);
    }
    for (const r of doomed) {
      const final = path.join(dataDir, `file_${r.id}.final`);
      if (fs.existsSync(final)) assert.equal(fs.readFileSync(final, 'utf8'), body(r.id, r.who));
    }
    assert.deepEqual(tmpFiles(), []);
  });

  // PHASE 3: STREAMING ------------------------------------------------------

  await check('THE SCENARIO: 30 x 100 MiB uploads at 10 KB/s, each client pulls the plug at a random 0.5-2.5 s', async () => {
    const clients = Array.from({ length: 30 }, (_, i) => {
      const target = `/blobs/${100 + i}`;
      const c = rawClient(port, `PUT ${target} HTTP/1.1\r\nHost: x\r\nContent-Length: ${100 * MiB}\r\n\r\n`);
      const drip = setInterval(() => c.sock.write(Buffer.alloc(1000, 0x61)), 100); // 10 KB/s
      setTimeout(() => { clearInterval(drip); c.sock.destroy(); }, 500 + Math.random() * 2000);
      return { ...c, target };
    });
    await sleep(450);
    const streaming = tmpFiles().filter((f) => /^blob_1\d\d\.tmp$/.test(f));
    const onDisk = streaming.map((f) => fs.statSync(path.join(dataDir, f)).size);
    const midway = await stats();
    console.log(`    at 450 ms: ${streaming.length} temp files filling from their sockets (${Math.min(...onDisk)}-${Math.max(...onDisk)} bytes each); Elm holds ${midway.inFlight} transactions`);
    assert.equal(streaming.length, 30, 'every upload streams into its own leased temp file');
    assert.ok(onDisk.every((n) => n > 0), 'bytes reach the disk while the client is still sending');
    await Promise.all(clients.map((c) => c.closed));
    await quiescent();
    const latencies = clients.map((c) => {
      const id = idOf.get(c.target);
      const cut = first(id, 'disconnect');
      const unlinked = of(id).find((e) => e.name === 'unlink:end' && e.at > cut.at);
      return ms(unlinked.at - cut.at);
    });
    console.log(`    disconnect -> temp file unlinked: p50 ${quantile(latencies, 0.5).toFixed(2)} ms, p99 ${quantile(latencies, 0.99).toFixed(2)} ms, max ${Math.max(...latencies).toFixed(2)} ms`);
    assert.ok(Math.max(...latencies) < 10, 'every abort was cleaned up within 10 ms');
    assert.deepEqual(tmpFiles(), []);
    assert.ok(!files().some((f) => /^blob_1\d\d\.bin$/.test(f)), 'nothing aborted was committed');
  });

  await check('SLOWLORIS (never hangs up): a 10 B/s upload is answered 408 and severed by the meter', async () => {
    const target = '/blobs/200';
    const c = rawClient(port, `PUT ${target} HTTP/1.1\r\nHost: x\r\nContent-Length: ${100 * MiB}\r\n\r\n`);
    const drip = setInterval(() => c.sock.write('xxxxxxxxxx'), 1000);
    await c.closed;
    clearInterval(drip);
    await quiescent();
    const id = idOf.get(target);
    const aborted = first(id, 'tx:abort');
    const unlinked = of(id).find((e) => e.name === 'unlink:end');
    const severed = ms(unlinked.at - aborted.at);
    console.log(`    the bridge got Elm's abort -> temp file unlinked: ${severed.toFixed(2)} ms; client saw ${JSON.stringify(c.text().split('\r\n')[0] || '(reset)')}`);
    assert.ok(severed < 10);
    assert.match(c.text(), /^HTTP\/1\.1 408 /, 'the client is told why before the connection closes');
    assert.deepEqual(tmpFiles(), []);
  });

  await check('an on-pace client is never called a slowloris because the meter read a stale count (L-spec Thm 14)', async () => {
    // 6000 bytes in two writes 20 ms apart, then a pause: the second write
    // lands inside the progress throttle. The meter must wait for the count.
    const c = rawClient(port, 'PUT /blobs/250 HTTP/1.1\r\nHost: x\r\nContent-Length: 12000\r\nExpect: 100-continue\r\nConnection: close\r\n\r\n');
    await waitFor('100 Continue', () => /100 Continue/.test(c.text()), 2000);
    c.sock.write(Buffer.alloc(3000, 0x61));
    await sleep(20);
    c.sock.write(Buffer.alloc(3000, 0x62));
    await sleep(1400);
    c.sock.write(Buffer.alloc(6000, 0x63));
    await c.closed;
    assert.match(c.text(), /HTTP\/1\.1 201 /, c.text().split('\r\n').filter((l) => l.startsWith('HTTP/')).join(' | '));
    assert.equal(fs.statSync(path.join(dataDir, 'blob_250.bin')).size, 12000);
  });

  await check('slowloris JSON: a trickled /echo body is answered 408 and the connection is closed', async () => {
    const c = rawClient(port, 'POST /echo HTTP/1.1\r\nHost: x\r\nContent-Length: 1000\r\n\r\n{"a":');
    const drip = setInterval(() => c.sock.write(' '), 300);
    await c.closed;
    clearInterval(drip);
    assert.match(c.text(), /^HTTP\/1\.1 408 [\s\S]*connection: close/i);
    await quiescent();
  });

  const bigId = 77;
  let bigDigest;
  await check('FULL SPEED: a 100 MiB upload streams to disk without the heap growing by the payload', async () => {
    global.gc?.();
    const before = process.memoryUsage().heapUsed;
    let peak = before;
    const sampler = setInterval(() => { peak = Math.max(peak, process.memoryUsage().heapUsed); }, 2);
    const digest = createHash('sha256');
    let sent = 0;
    const body = new Readable({
      read() {
        if (sent >= 100 * MiB) return this.push(null);
        const chunk = randomBytes(256 * 1024);
        digest.update(chunk);
        sent += chunk.length;
        return this.push(chunk);
      },
    });
    const t0 = Date.now();
    const r = await new Promise((resolve, reject) => {
      const req = http.request({ port, method: 'PUT', path: `/blobs/${bigId}`, agent: false, headers: { 'content-length': 100 * MiB } }, (res) => {
        let text = '';
        res.on('data', (c) => { text += c; });
        res.on('end', () => resolve({ status: res.statusCode, text }));
      });
      req.on('error', reject);
      body.pipe(req);
    });
    clearInterval(sampler);
    bigDigest = digest.digest('hex');
    const seconds = (Date.now() - t0) / 1000;
    console.log(`    ${(100 / seconds).toFixed(0)} MiB/s including fsync; peak heap +${((peak - before) / MiB).toFixed(1)} MiB for 100 MiB received`);
    assert.equal(r.status, 201, r.text);
    assert.deepEqual(JSON.parse(r.text), { blobId: bigId, bytes: 100 * MiB, sha256: bigDigest });
    assert.ok(peak - before < 32 * MiB, 'the body was not buffered');
    const back = await new Promise((resolve) => http.get({ port, path: `/blobs/${bigId}`, agent: false }, (res) => {
      const h = createHash('sha256');
      res.on('data', (c) => h.update(c));
      res.on('end', () => resolve(h.digest('hex')));
    }));
    assert.equal(back, bigDigest, 'the committed blob streams back byte-exact');
  });

  await check('refused from the headers: a declared 10 GiB upload gets 413 before one byte is read, even with Expect', async () => {
    const c = rawClient(port, `PUT /blobs/300 HTTP/1.1\r\nHost: x\r\nContent-Length: ${10 * 1024 ** 3}\r\nExpect: 100-continue\r\n\r\n`);
    await c.closed;
    assert.match(c.text(), /^HTTP\/1\.1 413 /);
    assert.doesNotMatch(c.text(), /100 Continue/, 'Elm never invited the body');
    assert.deepEqual(of(idOf.get('/blobs/300')), [], 'no transaction, no I/O');
  });

  await check('Expect: 100-continue is answered only once the transaction holds its lease', async () => {
    const body = randomBytes(1000);
    const c = rawClient(port, 'PUT /blobs/301 HTTP/1.1\r\nHost: x\r\nContent-Length: 1000\r\nExpect: 100-continue\r\nConnection: close\r\n\r\n');
    await waitFor('100 Continue', () => /100 Continue/.test(c.text()), 2000);
    assert.ok(first(idOf.get('/blobs/301'), 'open:end'), 'the lease was taken before the client was told to send');
    c.sock.write(body);
    await c.closed;
    assert.match(c.text(), /HTTP\/1\.1 201 /);
    assert.equal(sha256(fs.readFileSync(path.join(dataDir, 'blob_301.bin'))), sha256(body));
  });

  await check('a chunked upload that crosses the limit mid-stream is severed and rolled back', async () => {
    const small = start({ port: 0, dataDir, upstream: upstreamUrl, uploadLimit: MiB, log: () => {} });
    await listening(small.server);
    const r = await new Promise((resolve) => {
      const req = http.request({ port: small.server.address().port, method: 'PUT', path: '/blobs/302', agent: false, headers: { 'transfer-encoding': 'chunked' } }, (res) => {
        res.resume();
        res.on('end', () => resolve(res.statusCode));
      });
      req.on('error', (e) => resolve(e.code));
      let sent = 0;
      const pump = () => {
        while (sent < 3 * MiB) {
          sent += 64 * 1024;
          if (!req.write(randomBytes(64 * 1024))) return req.once('drain', pump);
        }
        return req.end();
      };
      pump();
    });
    await waitFor('the small server to settle', () => small.txs.size === 0 && small.handles.size === 0);
    small.server.close();
    console.log(`    client saw: ${r}`);
    assert.ok(r === 413 || r === 'ECONNRESET' || r === 'EPIPE', `unexpected ${r}`);
    assert.ok(!files().includes('blob_302.tmp') && !files().includes('blob_302.bin'));
  });

  await check('SLOW READER: a client that stops reading a 100 MiB download loses its fd to the delivery meter', async () => {
    const before = trail.length;
    await new Promise((resolve) => {
      const req = http.get({ port, path: `/blobs/${bigId}`, agent: false }, (res) => res.once('data', () => res.pause()));
      req.on('error', () => {});
      req.on('close', resolve);
      setTimeout(() => req.destroy(), 8000);
    });
    await quiescent();
    const id = trail.slice(before).find((e) => e.name === 'stream:start').request;
    const aborted = first(id, 'tx:abort');
    assert.ok(aborted, 'the meter aborted the stream; the client never hung up on its own');
    const closed = of(id).find((e) => e.name === 'close:end');
    console.log(`    Elm's abort -> fd closed: ${ms(closed.at - aborted.at).toFixed(2)} ms`);
    assert.ok(ms(closed.at - aborted.at) < 10);
  });

  await check('a FIFO planted as a blob is answered 500 at once: O_NONBLOCK, no thread parked in open(2)', async () => {
    const t0 = Date.now();
    assert.equal((await get('/blobs/7')).status, 500);
    assert.ok(Date.now() - t0 < 500);
    assert.equal(sha256((await get('/blobs/1')).raw), sha256(blob));
  });

  await check('STREAMING STORM: 40 uploads at once: 20 at full speed, 12 cut at random, 8 slowloris', async () => {
    const payloads = new Map();
    const fast = Array.from({ length: 20 }, (_, i) => {
      const id = 500 + i;
      const body = randomBytes(2 * MiB);
      payloads.set(id, body);
      return send(port, { method: 'PUT', path: `/blobs/${id}`, body }).then((r) => ({ id, r }));
    });
    const cut = Array.from({ length: 12 }, (_, i) => {
      const c = rawClient(port, `PUT /blobs/${600 + i} HTTP/1.1\r\nHost: x\r\nContent-Length: ${50 * MiB}\r\n\r\n`);
      const pump = setInterval(() => c.sock.write(randomBytes(64 * 1024)), 10);
      setTimeout(() => { clearInterval(pump); c.sock.destroy(); }, 50 + Math.random() * 1500);
      return c.closed;
    });
    const loris = Array.from({ length: 8 }, (_, i) => {
      const c = rawClient(port, `PUT /blobs/${700 + i} HTTP/1.1\r\nHost: x\r\nContent-Length: ${50 * MiB}\r\n\r\n`);
      const drip = setInterval(() => c.sock.write('z'), 500);
      return c.closed.then(() => clearInterval(drip));
    });
    const done = await Promise.all(fast);
    await Promise.all([...cut, ...loris]);
    await quiescent();
    for (const { id, r } of done) {
      assert.equal(r.status, 201);
      assert.equal(sha256(fs.readFileSync(path.join(dataDir, `blob_${id}.bin`))), sha256(payloads.get(id)));
    }
    assert.deepEqual(tmpFiles(), []);
    assert.ok(!files().some((f) => /^blob_[67]\d\d\.bin$/.test(f)), 'no cut or starved upload was committed');
  });

  // UNRECOGNIZED HOST ERRORS -------------------------------------------------

  await check('ELOOP on a blob is a fatal 500 with a logged fault, never a 404', async () => {
    const r = await get('/blobs/9');
    assert.deepEqual([r.status, json(r).fatal], [500, true]);
    assert.ok(faults.some((f) => f.event === 'unclassified' && f.error.code === 'ELOOP'));
  });

  await check('ENOTDIR (data dir is a file) is a fatal 500, never "missing"', async () => {
    const other = start({ port: 0, dataDir: path.join(dataDir, 'blob_1.bin'), upstream: upstreamUrl, log: () => {} });
    const otherFaults = [];
    other.events.on('fault', (f) => otherFaults.push(f));
    await listening(other.server);
    const r = await send(other.server.address().port, { method: 'POST', path: '/ingest/1', body: '{}' });
    other.server.close();
    assert.deepEqual([r.status, json(r).fatal], [500, true]);
    assert.equal(otherFaults[0].error.code, 'ENOTDIR');
  });

  // BLOBS --------------------------------------------------------------------

  await check('blob: 10 MiB byte-exact with exact Content-Length; missing 404, empty 200, directory 500', async () => {
    const before = trail.length;
    const r = await get('/blobs/1');
    assert.deepEqual([r.status, r.headers['content-length'], sha256(r.raw)], [200, String(blob.length), sha256(blob)]);
    await quiescent();
    assert.ok(!trail.slice(before).some((e) => e.name === 'disconnect'), 'a finished download is not a client that left');
    assert.equal((await get('/blobs/2')).status, 404);
    const empty = await get('/blobs/3');
    assert.deepEqual([empty.status, empty.raw.length], [200, 0]);
    assert.equal((await get('/blobs/4')).status, 500);
  });

  await check('blob: 20 concurrent 10 MiB downloads (200 MiB) without the heap growing by the payload', async () => {
    global.gc?.();
    const before = process.memoryUsage().heapUsed;
    let peak = before;
    const sampler = setInterval(() => { peak = Math.max(peak, process.memoryUsage().heapUsed); }, 2);
    const digests = await Promise.all(Array.from({ length: 20 }, () => new Promise((resolve, reject) => {
      http.get({ port, path: '/blobs/1', agent: false }, (res) => {
        const h = createHash('sha256');
        res.on('data', (c) => h.update(c));
        res.on('end', () => resolve(h.digest('hex')));
        res.on('error', reject);
      }).on('error', reject);
    })));
    clearInterval(sampler);
    for (const d of digests) assert.equal(d, sha256(blob));
    console.log(`    peak heap +${((peak - before) / MiB).toFixed(1)} MiB for 200 MiB served`);
    assert.ok(peak - before < 64 * MiB);
  });

  await check('autocannon: 20 connections x 5 s each on /delayed/42 and POST /ingest, zero errors', async () => {
    const delayed = await autocannon({ url: `http://localhost:${port}/delayed/42`, connections: 20, duration: 5 });
    let next = 10000;
    const ingest = await autocannon({
      url: `http://localhost:${port}`,
      connections: 20,
      duration: 5,
      requests: [{ method: 'POST', setupRequest: (req) => ({ ...req, path: `/ingest/${next++}`, body: `{"n":${next}}` }) }],
    });
    await quiescent();
    console.log(`    delayed: ${delayed['2xx']} x 2xx; ingest: ${ingest['2xx']} durable commits, p99 ${ingest.latency.p99} ms`);
    for (const r of [delayed, ingest]) assert.deepEqual([r.errors, r.timeouts, r.non2xx], [0, 0, 0]);
    assert.deepEqual(tmpFiles(), []);
  });

  await check('upstream down: ECONNREFUSED is a 502 naming the host error', async () => {
    upstream.server.close();
    await waitFor('upstream to close', () => !upstream.server.listening);
    const r = await get('/delayed/42');
    assert.deepEqual([r.status, json(r).hostError.code], [502, 'ECONNREFUSED']);
  });

  await check('no leaks: no exchanges, handles or transactions; ledger balanced; fds at baseline; faults accounted for', async () => {
    await quiescent();
    const s = await stats();
    console.log(`    ${JSON.stringify(s)}`);
    assert.deepEqual([app.exchanges.size, app.handles.size, app.txs.size], [0, 0, 0]);
    assert.equal(s.received, s.responded + s.abandoned);
    assert.deepEqual(tmpFiles(), []);
    assert.deepEqual([...new Set(faults.map((f) => f.event))].sort(), ['orphan', 'unclassified'], JSON.stringify(faults));
    assert.ok(openFds() <= baselineFds, `fds: ${openFds()} now, ${baselineFds} at start`);
  });

  app.server.closeAllConnections();
  app.server.close();
  await fsp.rm(dataDir, { recursive: true, force: true });
  console.log(`${checks} checks passed`);
  setTimeout(() => {
    console.error('FAIL process still alive 5 s after close: something leaked a handle');
    process.exit(1);
  }, 5000).unref();
}

main().catch((err) => {
  console.error('FAIL', err);
  process.exit(1);
});
