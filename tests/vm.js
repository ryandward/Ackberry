'use strict';

// Differential test: the bridge's transaction interpreter (server.js) against
// Elm's reference semantics (src/Tx.elm), on random programs.
//
// For each program, Elm builds it from the real Tx/Io types and answers with
// the exact instructions production code would send, plus the reports the
// reference interpreter says must come back. The real interpreter runs those
// instructions against scripted drivers, and every report must match.
//
//   node tests/vm.js [programs] [seed]

const { EventEmitter } = require('node:events');
const { createInterpreter } = require('../server.js');
const { Elm } = require('../build/tx-spec.js');

const spec = Elm.TxSpec.init().ports;
const ask = (program) => new Promise((resolve) => {
  const once = (value) => {
    spec.verdict.unsubscribe(once);
    resolve(value);
  };
  spec.verdict.subscribe(once);
  spec.program.send(program);
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

function generate(rand) {
  let steps = 0;
  let cleanups = 0;
  const pick = (xs) => xs[Math.floor(rand() * xs.length)];
  const op = (label) => ({ label, interruptible: rand() < 0.3 });
  const segmentCount = 1 + Math.floor(rand() * 3);
  const segments = [];
  for (let i = 0; i < segmentCount; i += 1) {
    const list = [];
    for (let j = Math.floor(rand() * 5); j > 0; j -= 1) {
      const commit = rand() < 0.15;
      const registered = [];
      if (!commit) {
        for (let k = Math.floor(rand() * 3); k > 0; k -= 1) {
          registered.push({ always: rand() < 0.5, op: op(`c${cleanups}`) });
          cleanups += 1;
        }
      }
      list.push({ commit, op: op(`s${steps}`), cleanups: registered });
      steps += 1;
    }
    segments.push({ steps: list, then: i === segmentCount - 1 ? 'finish' : 'pause' });
  }
  const outcomes = {};
  const code = () => (rand() < 0.8 ? null : pick(['EIO', 'ENOSPC', 'EACCES', 'EWEIRD']));
  for (let i = 0; i < steps; i += 1) outcomes[`s${i}`] = code();
  for (let i = 0; i < cleanups; i += 1) outcomes[`c${i}`] = code();
  const r = rand();
  const abort = r < 0.4 ? null
    : r < 0.8 ? { duringStep: Math.floor(rand() * (steps + 1)) }
      : { whilePaused: Math.floor(rand() * segmentCount) };
  return { segments, outcomes, abort };
}

const normalize = (report) => ({
  status: report.status,
  committed: report.committed,
  aborted: report.aborted,
  steps: report.steps.map((o) => ('ok' in o ? 'ok' : `err:${o.error.code}`)),
  cleanups: report.cleanups.map((c) => `${c.kind}|${c.path ?? ''}|${'ok' in c.outcome ? 'ok' : `err:${c.outcome.error.code}`}`),
});

function runInterpreter(program, instructions) {
  return new Promise((resolveRun, reject) => {
    // Not unref'd: if the interpreter deadlocks, nothing else keeps the event
    // loop alive, and Node would exit 0 with this promise still pending.
    const timer = setTimeout(() => reject(new Error('the interpreter never settled')), 2000);
    const resolve = (value) => {
      clearTimeout(timer);
      resolveRun(value);
    };
    const reports = [];
    let pauses = 0;
    let next = 1;
    let vm;
    const extendNext = () => {
      const instruction = instructions[next] ?? { do: 'extend', tx: 'tx', steps: [], then: 'finish' };
      next += 1;
      vm.extend(instruction);
    };
    const scripted = async (label, ctx, interruptible) => {
      if (label.startsWith('s') && program.abort?.duringStep === Number(label.slice(1))) vm.abort({ tx: 'tx' });
      await new Promise(setImmediate); // a host round trip
      if (interruptible && ctx.signal.aborted) throw Object.assign(new Error('This operation was aborted'), { name: 'AbortError', code: 'ABORT_ERR' });
      const code = program.outcomes[label];
      if (code) throw Object.assign(new Error(code), { code });
      return {};
    };
    vm = createInterpreter({
      drivers: {
        unlink: (op, ctx) => scripted(op.path, ctx, false),
        httpGet: (op, ctx) => scripted(op.url, ctx, true),
      },
      report: (_request, report) => {
        reports.push(normalize(report));
        if (report.status === 'settled') return resolve(reports);
        const abortHere = program.abort?.whilePaused === pauses;
        pauses += 1;
        if (abortHere) return vm.abort({ tx: 'tx' });
        return extendNext();
      },
      progress: () => {},
      reclaim: async () => [],
      events: new EventEmitter(),
    });
    vm.begin(instructions[0]);
  });
}

async function main() {
  const programs = Number(process.argv[2] ?? 5000);
  const seed = Number(process.argv[3] ?? Date.now() % 2 ** 31);
  const rand = prng(seed);
  const tally = { committed: 0, aborted: 0, paused: 0, cleanups: 0 };
  for (let i = 0; i < programs; i += 1) {
    const program = generate(rand);
    const answer = await ask(program);
    if (answer.error) throw new Error(`Elm could not read program ${i}: ${answer.error}`);
    const actual = await runInterpreter(program, answer.instructions);
    if (JSON.stringify(actual) !== JSON.stringify(answer.reports)) {
      console.error(`MISMATCH on program ${i} (seed ${seed})`);
      console.error('program  ', JSON.stringify(program));
      console.error('reference', JSON.stringify(answer.reports));
      console.error('bridge   ', JSON.stringify(actual));
      process.exit(1);
    }
    const last = actual[actual.length - 1];
    tally.committed += last.committed ? 1 : 0;
    tally.aborted += last.aborted ? 1 : 0;
    tally.paused += actual.some((r) => r.status === 'paused') ? 1 : 0;
    tally.cleanups += last.cleanups.length > 0 ? 1 : 0;
  }
  console.log(`interpreter == reference on ${programs} random programs (seed ${seed}): ${JSON.stringify(tally)}`);
}

main().catch((err) => {
  console.error('FAIL', err);
  process.exit(1);
});
