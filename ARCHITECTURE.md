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
| every deadline: the head, the idle keep-alive, the transfer, the linger | nothing; it has no clocks |
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

## Plan

1. `Http.Head`: the head parser, pure, with conformance tests. Done.
2. Response serialization in Elm: status line, `Content-Length` in UTF-8
   bytes, `Date`, `connection: close`, `100 Continue`.
3. The connection state machine in `Server`, pure and tested: awaiting a
   head under a deadline, one exchange at a time, then persist, dissipate or
   sever; an idle deadline between requests.
4. The `node:net` bridge: delimiting, the chunked decoder with its Elm
   reference and differential test, writes, the staged close without Node
   internals.
5. Switch over. The acceptance suite is the behavioral contract; new checks
   cover persistence, pipelining, and every deadline being Elm's.
