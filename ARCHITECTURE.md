# Ackberry architecture

## Decision: the bridge owns the socket, Elm owns HTTP

`node:http` is evicted. The bridge sits on `node:net`, and Elm owns each
connection from its first byte to its last.

`node:http` was never a neutral transport. It enforces its own request,
header and keep-alive timeouts (300 s, 60 s and 5 s by default), its own
header limits, and its own refusals of malformed heads, and it answers those
requests itself, before Elm has seen them. When its request timeout fired on
an upload that was keeping Elm's pace, it sent the client a 408, and Elm,
told only that the socket closed, recorded a client that walked away. Two
state machines, disagreeing about what happened, is the defect; the
monkey-patches the staged close needed (`destroySoon`, the parser takeover,
`_handle.readStart`, the null `req.socket`) were symptoms of it.

## Who does what

| Elm decides | The bridge does, and reports |
| --- | --- |
| what a head means: request line, fields, framing, persistence, `Expect` (`src/Http/Head.elm`, RFC 9112) | accept, read bytes, find the end of a head within Elm's byte limit |
| every status line, every header, every body byte it sends, `100 Continue` included | write the bytes Elm serialized |
| every deadline: the head, the idle keep-alive, the transfer, the linger | stamp each event with its monotonic and wall clocks; fire the one alarm Elm sets; enforce the linger bounds Elm hands it |
| whether a connection persists, dissipates, or is severed, and the linger pool | close when told; drop bytes within Elm's bounds |
| what a host error means | run storage transactions under the laws in `src/Tx.elm` |
| the chunked body's limits | decode chunked framing mechanically, checked against an Elm reference decoder |

The bridge reports facts with their times (a head arrived, the peer closed,
a fd was released, a step's outcome) and decides nothing. Body bytes never
cross into Elm except small JSON bodies a step reads on purpose.

## Rejected

- **eBPF or XDP as the executor.** XDP sees packets before the TCP stack, so
  it has no byte stream; eBPF cannot call `link(2)` or `fsync`; both need
  privileges this project's tests are designed to run without.
- **An assembly or native executor now.** Elm compiles to JavaScript, not
  machine code, so a native executor would interpret the same instructions
  the Node bridge does. It becomes worth writing once the instruction set
  stops changing, and it must pass the same differential tests
  (`tests/vm.js`) against Elm's reference semantics. `io_uring` linked
  submissions would carry Tx laws 1 and 3 natively; compensation (laws 2
  and 5) still needs the executor to observe failures.
- **Keeping `node:http` and configuring it into silence.** The request
  timeout, the parser's refusals and the response lifecycle cannot all be
  disabled, and the socket would still not be ours.

## The clock

Elm 0.19.1 has no monotonic clock: `Time.now` and `Time.every` both read
`Date.now()`, which jumps when the wall clock is set. So Elm keeps no timers.
Every event the bridge reports carries `mono` (`performance.now()`) and
`wall` (`Date.now()`), read as the event leaves the bridge. Deadlines are
arithmetic on `mono`; only the `Date` header reads `wall`, floored to the
second. Elm asks for at most one alarm at a time: every `tickMs` while a
transfer is metered or a connection awaits a head, else at the next sweep,
else none. When it fires, Elm reads the time it carries and expires what is
due.

## Decisions made on the way

- **A FIN in the middle of an exchange is the client leaving.** HTTP lets
  a client half-close after sending its request and still read the answer.
  But from the server's side, a client that has ended its side looks exactly
  like one that has gone until a write to it fails, which for a long upload
  can be minutes later, and the transactions here are long and abortable.
  So the exchange is aborted, and its compensations run. A client that
  half-closes before its answer loses the answer; that is the deviation.
- **Hold or cut is decided per connection.** A dissipated connection whose
  budget is spent is held to the deadline while at least half the pool would
  stay free, and cut at once under pressure (measured trade-off in
  `src/Kernel.elm`).
- **`100 Continue` rides on the body step.** The invitation is written by
  the step that reads or pipes the body, after the transaction holds its
  lease, so a refusal from the lease is sent instead of an invitation, and
  no step position shifts.
- **Requests are numbered on their connection.** A request id is
  `<connection>.<n>`, so a part's name says which connection's which request
  wrote it.

## Plan

All five steps are done.

1. `Http.Head`: the head parser, pure, with conformance tests
   (`tests/HeadTest.elm`, 63 tests; 23 of 25 mutants caught, and the 2
   equivalent ones exposed checks that were redundant and were deleted).
2. Response serialization in Elm (`Http.Response.serialize`): status line
   and reason phrase from one exhaustive case, `Content-Length` counted in
   UTF-8 bytes, `Date` in IMF-fixdate, `connection: close`, and
   `100 Continue` as its own constant.
3. The connection state machine in `Server`: a connection awaits a head
   under the head deadline (the first) or the idle deadline (the next), then
   carries one exchange, then persists, dissipates, or is severed.
   `tests/WorldTest.elm` drives it against a simulated host with random
   interleavings of heads, hang-ups, half-closes and fd releases.
4. The `node:net` bridge: head delimiting, a chunked decoder checked against
   `Http.Chunked` by `tests/chunked.js` (seeded random bodies split at
   random; 21 of 21 mutants across both decoders caught), and the staged
   close without Node internals.
5. Switched over. The acceptance suite passes against the `node:net`
   runtime, with new checks for keep-alive, pipelining behind a chunked
   body, both head deadlines, and refused heads answered by Elm.
