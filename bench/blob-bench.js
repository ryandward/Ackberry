'use strict';

// Where does carrying binary through Elm ports stop working?
//
// Each (strategy, size) runs in a fresh child process so peak RSS comes from
// the kernel (getrusage maxrss) and an out-of-memory crash is a data point,
// not the end of the run. "Overhead" is peak RSS minus RSS just before the
// transfer, with the source buffer already allocated: what the transport
// itself costs.
//
//   node bench/blob-bench.js            (after: npm run build && npm run build:bench)

const { fork } = require('node:child_process');
const fs = require('node:fs');
const http = require('node:http');
const path = require('node:path');
const { createHash, randomBytes } = require('node:crypto');

const MiB = 1024 * 1024;
const sha256 = (bytes) => createHash('sha256').update(bytes).digest('hex');

async function child(strategy, mib) {
  const bytes = randomBytes(mib * MiB);
  const digest = sha256(bytes);
  let run;

  if (strategy === 'stream') {
    const { start } = require('../server.js');
    const dir = fs.mkdtempSync(path.join(__dirname, '..', '.test-data', 'bench-'));
    fs.writeFileSync(path.join(dir, 'blob_1.bin'), bytes);
    const app = start({ port: 0, dataDir: dir, upstream: 'http://127.0.0.1:9', log: () => {} });
    await new Promise((resolve) => app.server.on('listening', resolve));
    run = () => new Promise((resolve, reject) => {
      http.get({ port: app.server.address().port, path: '/blobs/1', agent: false }, (res) => {
        const hash = createHash('sha256');
        res.on('data', (c) => hash.update(c));
        res.on('end', () => {
          app.server.close();
          fs.rmSync(dir, { recursive: true, force: true });
          resolve(hash.digest('hex'));
        });
      }).on('error', reject);
    });
  } else {
    const { Elm } = require('../build/blob-bench.js');
    const ports = Elm.BlobBench.init().ports;
    const via = {
      list: ['listIn', 'listOut', () => Array.from(bytes), (out) => Buffer.from(out)],
      array: ['arrayIn', 'arrayOut', () => Array.from(bytes), (out) => Buffer.from(out)],
      base64: ['base64In', 'base64Out', () => bytes.toString('base64'), (out) => Buffer.from(out, 'base64')],
    }[strategy];
    const [inPort, outPort, encode, decode] = via;
    run = () => new Promise((resolve) => {
      ports[outPort].subscribe((out) => resolve(sha256(decode(out))));
      ports[inPort].send(encode());
    });
  }

  const before = process.memoryUsage().rss;
  const t0 = process.hrtime.bigint();
  const got = await run();
  const ms = Number(process.hrtime.bigint() - t0) / 1e6;
  const peak = process.resourceUsage().maxRSS * 1024;
  process.send({ ok: got === digest, ms, overheadMiB: (peak - before) / MiB });
  process.exit(0);
}

function measure(strategy, mib) {
  return new Promise((resolve) => {
    const proc = fork(__filename, ['child', strategy, String(mib)], { stdio: ['ignore', 'ignore', 'pipe', 'ipc'] });
    let stderr = '';
    let result = null;
    const timer = setTimeout(() => proc.kill('SIGKILL'), 180000);
    proc.stderr.on('data', (d) => { stderr += d; });
    proc.on('message', (m) => { result = m; });
    proc.on('exit', (code, signal) => {
      clearTimeout(timer);
      if (result) return resolve(result);
      const why = /heap out of memory/.test(stderr) ? 'V8 heap exhausted'
        : /Invalid string length|ERR_STRING_TOO_LONG|longer than 0x/.test(stderr) ? 'V8 max string length'
        : /Invalid array length/.test(stderr) ? 'V8 max array length'
        : signal === 'SIGKILL' ? 'killed after 180 s' : `exit ${code ?? signal}`;
      resolve({ failed: why });
    });
  });
}

async function parent() {
  fs.mkdirSync(path.join(__dirname, '..', '.test-data'), { recursive: true });
  const plan = {
    list: [1, 10, 25, 50, 100, 200],
    array: [1, 10, 25, 50, 100, 200],
    base64: [1, 10, 100, 256, 384, 400],
    stream: [1, 10, 100, 1024],
  };
  console.log(`heap limit ${(require('node:v8').getHeapStatistics().heap_size_limit / MiB).toFixed(0)} MiB\n`);
  console.log('strategy  size MiB   time ms  overhead MiB  overhead/payload');
  for (const [strategy, sizes] of Object.entries(plan)) {
    for (const mib of sizes) {
      const r = await measure(strategy, mib);
      if (r.failed) {
        console.log(`${strategy.padEnd(8)} ${String(mib).padStart(9)}   FAILED: ${r.failed}`);
        break;
      }
      if (!r.ok) throw new Error(`${strategy} ${mib} MiB: bytes came back different`);
      console.log(`${strategy.padEnd(8)} ${String(mib).padStart(9)} ${r.ms.toFixed(0).padStart(9)} ${r.overheadMiB.toFixed(0).padStart(13)} ${(r.overheadMiB / mib).toFixed(1).padStart(16)}x`);
    }
  }
}

if (process.argv[2] === 'child') child(process.argv[3], Number(process.argv[4]));
else parent();
