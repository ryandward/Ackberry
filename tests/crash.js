'use strict';

// SIGKILL against the real server process. An upload killed mid-body
// leaves a corpse in the parts directory; the corpse must block nothing,
// and the janitor must collect it once it is stale.
//
//   node tests/crash.js

const { spawn } = require('node:child_process');
const fs = require('node:fs');
const fsp = require('node:fs/promises');
const http = require('node:http');
const net = require('node:net');
const path = require('node:path');
const assert = require('node:assert/strict');
const { createHash, randomBytes } = require('node:crypto');

const MiB = 1024 * 1024;
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');

async function waitFor(what, predicate, timeoutMs = 10000) {
  const deadline = Date.now() + timeoutMs;
  while (!(await predicate())) {
    if (Date.now() > deadline) throw new Error(`timed out waiting for ${what}`);
    await sleep(5);
  }
}

// `node server.js` as its own process, as in production. Resolves once it
// listens, which it does only after its data directory is prepared.
function boot(dataDir) {
  const child = spawn(process.execPath, [path.join(__dirname, '..', 'server.js')], {
    env: { ...process.env, DATA_DIR: dataDir, PORT: '0' },
    stdio: ['ignore', 'pipe', 'inherit'],
  });
  const exited = new Promise((resolve) => child.on('exit', (code, signal) => resolve({ code, signal })));
  return new Promise((resolve, reject) => {
    let out = '';
    child.stdout.on('data', (chunk) => {
      out += chunk;
      const listening = /listening on http:\/\/localhost:(\d+)/.exec(out);
      if (listening) resolve({ child, exited, port: Number(listening[1]) });
    });
    exited.then(({ code, signal }) => reject(new Error(`the server exited before it listened: ${code ?? signal}`)));
  });
}

function send(port, { method = 'GET', path: target, body }) {
  return new Promise((resolve, reject) => {
    const headers = body === undefined ? {} : { 'content-length': Buffer.byteLength(body) };
    const req = http.request({ port, method, path: target, headers, agent: false }, (res) => {
      const chunks = [];
      res.on('data', (c) => chunks.push(c));
      res.on('end', () => resolve({ status: res.statusCode, text: Buffer.concat(chunks).toString('utf8') }));
      res.on('error', reject);
    });
    req.on('error', reject);
    req.setTimeout(10000, () => req.destroy(new Error(`no response within 10 s: ${method} ${target}`)));
    if (body !== undefined) req.write(body);
    req.end();
  });
}

async function main() {
  // Not os.tmpdir(): /tmp is often tmpfs, where fsync is a no-op.
  const root = process.env.ACKBERRY_TEST_DIR ?? path.join(__dirname, '..', '.test-data');
  await fsp.mkdir(root, { recursive: true });
  const dataDir = await fsp.mkdtemp(path.join(root, 'crash-'));
  const parts = path.join(dataDir, '.tmp');
  const blob9 = path.join(dataDir, 'blobs', '9');
  let server = await boot(dataDir);

  try {
    // 1. SIGKILL while PUT /blobs/9 is streaming to disk.
    const sock = net.connect(server.port, '127.0.0.1');
    sock.on('error', () => {});
    const severed = new Promise((resolve) => sock.on('close', resolve));
    sock.write(`PUT /blobs/9 HTTP/1.1\r\nHost: x\r\nContent-Length: ${100 * MiB}\r\n\r\n`);
    const chunk = randomBytes(64 * 1024);
    let pumping = true;
    const pump = () => {
      while (pumping && sock.write(chunk));
      if (pumping) sock.once('drain', pump);
    };
    pump();
    const onDisk = () => fs.readdirSync(parts).map((name) => fs.statSync(path.join(parts, name)).size);
    await waitFor('8 MiB of the body on disk', () => onDisk().some((n) => n >= 8 * MiB));
    server.child.kill('SIGKILL');
    const death = await server.exited;
    pumping = false;
    await severed;
    assert.equal(death.signal, 'SIGKILL');

    const corpses = fs.readdirSync(parts);
    assert.equal(corpses.length, 1, `one corpse expected, found ${corpses}`);
    assert.match(corpses[0], /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\.part$/, 'named after the dead transaction, not after blob 9');
    const corpse = path.join(parts, corpses[0]);
    const corpseBytes = fs.statSync(corpse).size;
    assert.ok(!fs.existsSync(blob9), 'nothing was committed');
    console.log(`SIGKILL during PUT /blobs/9: the process died by ${death.signal} with ${(corpseBytes / MiB).toFixed(1)} of 100 MiB on disk`);
    console.log(`  left behind: .tmp/${corpses[0]} (${corpseBytes} bytes); blobs/9 does not exist`);

    // 2. Restart and retry the same id at once.
    const t0 = Date.now();
    server = await boot(dataDir);
    const bootMs = Date.now() - t0;
    const missing = await send(server.port, { path: '/blobs/9' });
    assert.equal(missing.status, 404, missing.text);
    const body = randomBytes(MiB);
    const t1 = Date.now();
    const retry = await send(server.port, { method: 'PUT', path: '/blobs/9', body });
    const retryMs = Date.now() - t1;
    assert.equal(retry.status, 201, retry.text);
    assert.deepEqual(JSON.parse(retry.text), { blobId: 9, bytes: MiB, sha256: sha256(body) });
    assert.equal(sha256(fs.readFileSync(blob9)), sha256(body));
    assert.ok(fs.existsSync(corpse), 'the corpse is minutes old, so the janitor left it; it blocked nothing');
    console.log(`restart: listening after ${bootMs} ms; GET /blobs/9 -> ${missing.status}; PUT /blobs/9 (1 MiB) -> ${retry.status} in ${retryMs} ms`);
    console.log(`  the corpse is still in .tmp/, untouched and in nobody's way`);

    // 3. 409 now has exactly one meaning: the id is taken.
    const again = await send(server.port, { method: 'PUT', path: '/blobs/9', body: 'a second upload' });
    assert.equal(again.status, 409, again.text);
    assert.deepEqual(JSON.parse(again.text).hostError, { code: 'EEXIST', syscall: 'link' });
    console.log(`PUT /blobs/9 again -> ${again.status}: ${JSON.parse(again.text).detail}`);

    // 4. Once the corpse is stale, the next boot's sweep removes it.
    server.child.kill('SIGKILL');
    await server.exited;
    const threeHoursAgo = new Date(Date.now() - 3 * 3600 * 1000);
    fs.utimesSync(corpse, threeHoursAgo, threeHoursAgo);
    server = await boot(dataDir);
    await waitFor('the boot sweep to remove the corpse', () => !fs.existsSync(corpse));
    const stats = JSON.parse((await send(server.port, { path: '/stats' })).text);
    assert.equal(stats.swept, 1);
    assert.equal(sha256(fs.readFileSync(blob9)), sha256(body), 'the committed blob is untouched');
    assert.deepEqual(fs.readdirSync(parts), []);
    console.log(`corpse backdated 3 h, SIGKILL, restart: the boot sweep removed it (swept ${stats.swept}); blobs/9 intact`);
  } finally {
    server.child.kill('SIGKILL');
    await server.exited;
    await fsp.rm(dataDir, { recursive: true, force: true });
  }
  console.log('crash test passed');
}

main().catch((err) => {
  console.error('FAIL', err);
  process.exit(1);
});
