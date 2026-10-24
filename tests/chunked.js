'use strict';

// Differential test: the bridge's chunked decoder (server.js) against Elm's
// reference (src/Http/Chunked.elm), on random chunked bodies and corrupted
// ones. The bridge's decoder gets each input split at random places, as a
// socket would deliver it; both must agree on the outcome, the data, the
// bytes the framing took, and the offset of any error.
//
//   node tests/chunked.js [cases] [seed]

const { ChunkedDecoder } = require('../server.js');
const { Elm } = require('../build/chunked-spec.js');

const spec = Elm.ChunkedSpec.init().ports;
const ask = (batch) => new Promise((resolve) => {
  const once = (value) => {
    spec.verdicts.unsubscribe(once);
    resolve(value);
  };
  spec.verdicts.subscribe(once);
  spec.cases.send(batch);
});

// mulberry32: a seeded PRNG, so any failure can be replayed.
function prng(seed) {
  let a = seed >>> 0;
  return () => {
    a = (a + 0x6d2b79f5) >>> 0;
    let t = a;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

// A chunked body, maybe followed by a pipelined request, then maybe
// corrupted. The bytes that matter are drawn often: sizes padded to either
// side of the 13-digit limit, extensions holding the controls that must
// break them, whitespace that no extension follows, bare LFs, zero-chunks
// spelled with several zeros, trailers with controls, bare CRs and folds,
// and limits small enough that the boundary byte is often the one decoded.
function generate(rand) {
  const int = (lo, hi) => lo + Math.floor(rand() * (hi - lo + 1));
  const pick = (xs) => xs[int(0, xs.length - 1)];
  const bytes = [];
  const text = (s) => { for (const ch of s) bytes.push(ch.charCodeAt(0)); };
  const extension = () => pick([';a', ';a=b', ';n="q v"', ';x=\t1', `;e=${String.fromCharCode(int(0x80, 0xff))}`, ';;', ';=', ';a=\x01', ';a=\x7f', ';a\nb']);
  const whitespace = () => pick([' ', '\t', '  ', ' \t']);
  const size = (n) => {
    let hex = n.toString(16);
    if (rand() < 0.3) hex = hex.toUpperCase();
    const pad = rand();
    if (pad < 0.1) hex = '0'.repeat(int(1, 3)) + hex;
    else if (pad < 0.15) hex = '0'.repeat(Math.max(0, int(11, 15) - hex.length)) + hex;
    return hex;
  };
  const sizeLine = (hex) => {
    text(hex);
    const after = rand();
    if (after < 0.2) text(extension());
    else if (after < 0.3) text(whitespace() + extension());
    else if (after < 0.33) text(whitespace());
    text(rand() < 0.03 ? '\n' : '\r\n');
  };
  if (rand() < 0.03) {
    // A 13-digit size, more than 2^51, with too little data: the size must
    // be read exactly, and the body is incomplete. One digit more is malformed.
    sizeLine(pick(['fffffffffffff', '1000000000000', 'FFFFFFFFFFFFF0', '0000000000001']));
    text('abc');
  } else {
    for (let c = int(0, 4); c > 0; c -= 1) {
      const n = int(1, 40);
      sizeLine(size(n));
      for (let i = 0; i < n; i += 1) bytes.push(int(0, 255));
      text('\r\n');
    }
    sizeLine('0'.repeat(int(1, 3)));
    for (let t = int(0, 2); t > 0; t -= 1) text(`${pick([`X-T${t}: v${int(0, 99)}`, 'X: a\x01b', ' folded', 'X: a\rb', 'X:', 'X: \x7f'])}\r\n`);
    text(rand() < 0.03 ? '\n' : '\r\n');
    if (rand() < 0.3) text('GET / HTTP/1.1\r\n');
  }
  const m = rand();
  if (m < 0.15 && bytes.length > 0) bytes[int(0, bytes.length - 1)] = int(0, 255);
  else if (m < 0.25 && bytes.length > 0) bytes.splice(int(0, bytes.length - 1), 1);
  else if (m < 0.35) bytes.splice(int(0, bytes.length), 0, int(0, 255));
  else if (m < 0.45) bytes.length = int(0, bytes.length);
  else if (m < 0.5) {
    bytes.length = 0;
    for (let i = int(0, 60); i > 0; i -= 1) bytes.push(pick([0x30, 0x31, 0x61, 0x3b, 0x20, 0x0d, 0x0a, int(0, 255)]));
  }
  return {
    input: String.fromCharCode(...bytes),
    maxLine: rand() < 0.5 ? int(1, 24) : 8192,
    maxTrailer: rand() < 0.5 ? int(0, 48) : 16384,
  };
}

function decodeSplit({ input, maxLine, maxTrailer }, rand) {
  const buf = Buffer.from(input, 'latin1');
  const decoder = new ChunkedDecoder({ maxLine, maxTrailer });
  const data = [];
  const most = rand() < 0.2 ? buf.length : 1 + Math.floor(rand() * 17);
  let pos = 0;
  try {
    while (pos < buf.length) {
      const n = 1 + Math.floor(rand() * Math.min(buf.length - pos, most));
      const out = decoder.push(buf.subarray(pos, pos + n));
      data.push(...out.data);
      if (out.done) return { kind: 'complete', data: Buffer.concat(data).toString('latin1'), used: pos + out.used };
      if (out.used !== n) throw new Error(`the decoder stopped at ${out.used} of ${n} bytes without finishing`);
      pos += n;
    }
    return { kind: 'incomplete' };
  } catch (err) {
    if (err.code !== 'ACKBERRY_FRAMING') throw err;
    return { kind: 'malformed', at: err.offset };
  }
}

async function main() {
  const count = Number(process.argv[2] ?? 5000);
  const seed = Number(process.argv[3] ?? Math.floor(Math.random() * 2 ** 31));
  const rand = prng(seed);
  const batch = Array.from({ length: count }, () => generate(rand));
  const expected = await ask(batch);
  if (!Array.isArray(expected)) throw new Error(`the Elm reference refused the batch: ${JSON.stringify(expected)}`);
  const tally = { complete: 0, incomplete: 0, malformed: 0 };
  batch.forEach((c, i) => {
    const got = decodeSplit(c, rand);
    const want = expected[i];
    if (JSON.stringify(got) !== JSON.stringify(want)) {
      console.error(`case ${i} (seed ${seed}): ${JSON.stringify(c)}`);
      console.error(`  bridge:    ${JSON.stringify(got)}`);
      console.error(`  reference: ${JSON.stringify(want)}`);
      process.exit(1);
    }
    tally[got.kind] += 1;
  });
  console.log(`chunked decoder == reference on ${count} bodies split at random (seed ${seed}): ${JSON.stringify(tally)}`);
}

main().catch((err) => {
  console.error('FAIL', err);
  process.exit(1);
});
