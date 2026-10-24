module Kernel exposing
    ( Answer
    , Arrival(..)
    , Dissipation
    , Drained
    , Ending
    , Exchange
    , KernelTrace
    , Pool
    , Release(..)
    , Unread
    , WhenSpent(..)
    , Witness(..)
    , answer
    , answerAndClose
    , arrive
    , drained
    , encode
    , held
    , pool
    , preflight
    , refusal
    , releaseDecoder
    , released
    , respondAfter
    , streamedAfter
    , witness
    , yieldAfter
    )

{-| The socket's side of every exchange, written by Elm as data and run by
the bridge.

Three decisions the bridge makes today move here:

  - Whether a connection may stay open. An exchange is typed by what is
    known about its body, and only an `Exchange Drained` can be answered
    without closing. It is the unread body that forbids keep-alive, not the
    status: a 200 to an unread body must close as surely as a 409 does.
  - How a connection that must close ends. Every answer to an
    `Exchange Unread` carries a `Dissipation` or is severed; there is no
    third way.
  - Whether the linger pool has room. `Pool` is Elm's own exact count: a
    slot is held from the moment Elm decides to dissipate until the bridge
    reports the fd closed, so it never undercounts, and the bridge never
    compares anything with a cap.

What this cannot do:

  - Make the bridge's execution branch-free. A dissipation waits on a peer,
    and "until EOF, the deadline or the budget" has three exits. The trace
    is a closed instruction set, and the bridge's only branches are on which
    declared outcome happened, never on what one means.
  - Remove the Node internals the staged close patches (`destroySoon`, the
    parser takeover, `_handle.readStart`). Those exist because `node:http`
    owns the socket; only taking the socket from `node:http` removes them.
  - Bind a value to its source. Elm has no linear types: `drained` trusts
    that the outcome it is given is the body step's, and `respondAfter`
    trusts that its program finishes rather than pauses.

The bridge must obey these laws:

1.  A trace's program runs under the laws in `Tx`.
2.  `Respond` is carried out only after the program finishes, and its fate
    only after the answer's bytes are flushed.
3.  `KeepAlive` does nothing. `Sever` closes at once. `Dissipate` sends FIN,
    then reads and drops, never parsing, until EOF, `deadlineMs`, or
    `budgetBytes`; at the budget, `Cut` closes and `Hold` stops reading
    and closes at the deadline.
4.  Every `Dissipate` is reported exactly once, after its fd is closed,
    including `Gone` when the exchange vanished before its answer could be
    written. A missing report would hold its slot forever.
5.  The bridge never chooses a fate and never compares a count with a cap.

-}

import Http.Request as Request exposing (Request, RequestId)
import Http.Response as Response exposing (Response)
import Http.Route as Route exposing (Route)
import Io exposing (HostError)
import Json.Decode as Decode
import Json.Encode as Encode
import Layout exposing (Final, Layout)
import Plan
import Set exposing (Set)
import Tx



-- EXCHANGES


type Exchange body
    = Exchange RequestId


{-| The request declared a body that nobody has read.
-}
type Unread
    = Unread


{-| The request had no body, or a step read all of it.
-}
type Drained
    = Drained


type Arrival
    = Bodiless (Exchange Drained)
    | Bodied (Exchange Unread)


{-| A declared length of zero is a body already read to its end.
-}
arrive : RequestId -> Request -> Arrival
arrive id request =
    case request.body of
        Request.NoBody ->
            Bodiless (Exchange id)

        Request.Length 0 ->
            Bodiless (Exchange id)

        Request.Length _ ->
            Bodied (Exchange id)

        Request.Unsized ->
            Bodied (Exchange id)


{-| The only way to a drained exchange: the outcome of the step that read
the body, and it succeeded.
-}
drained : Result HostError a -> Exchange Unread -> Maybe (Exchange Drained)
drained outcome (Exchange id) =
    case outcome of
        Ok _ ->
            Just (Exchange id)

        Err _ ->
            Nothing



-- FATES


{-| The staged close of RFC 9112 section 9.6. `budgetBytes` stops a fast
sender; `deadlineMs` stops a slow one, which never comes near the budget.
-}
type alias Dissipation =
    { deadlineMs : Int
    , budgetBytes : Int
    , whenSpent : WhenSpent
    }


{-| What happens once the budget is spent. Measured on loopback against
clients still sending when the refusal arrived:

`Cut` closes at once and frees the fd and the slot. A loopback sender
spends the budget in microseconds, so the close comes barely later than an
immediate one: curl got its answer 399 times in 400, Node's client 168 in 250.

`Hold` stops reading and closes at the deadline. The client's sends block
instead of failing, so it has time to read: curl 400 in 400, Node's client
239 in 250. But a socket that is not read cannot see its peer hang up, so
every fast sender holds its slot for the whole deadline: with 50 senders at
a time, a 64-slot pool overflowed 141 times in 700.

-}
type WhenSpent
    = Cut
    | Hold


type Fate
    = KeepAlive
    | Dissipate Dissipation
    | Sever


type Answer
    = Answer RequestId Response Fate


{-| A drained exchange may stay open, whatever the status.
-}
answer : Response -> Exchange Drained -> Answer
answer response (Exchange id) =
    Answer id response KeepAlive


{-| An unread exchange closes, whatever the status: dissipated if the pool
has a slot, severed if it does not. The pool comes back with the slot taken.
-}
answerAndClose : Dissipation -> Response -> Exchange Unread -> Pool -> ( Answer, Pool )
answerAndClose profile response (Exchange id) (Pool p) =
    if Set.size p.held < p.cap then
        ( Answer id response (Dissipate profile), Pool { p | held = Set.insert (Request.idToString id) p.held } )

    else
        ( Answer id response Sever, Pool p )



-- THE POOL


type Pool
    = Pool { cap : Int, held : Set String }


pool : Int -> Pool
pool cap =
    Pool { cap = cap, held = Set.empty }


held : Pool -> Int
held (Pool p) =
    Set.size p.held


{-| How a dissipated connection ended, reported once its fd is closed.
`BudgetSpent` covers both `Cut` and a `Hold` that ran to the deadline.
-}
type Release
    = HungUp
    | DeadlinePassed
    | BudgetSpent
    | Gone


releaseDecoder : Decode.Decoder Release
releaseDecoder =
    Decode.string
        |> Decode.andThen
            (\how ->
                case how of
                    "hungUp" ->
                        Decode.succeed HungUp

                    "deadline" ->
                        Decode.succeed DeadlinePassed

                    "budget" ->
                        Decode.succeed BudgetSpent

                    "gone" ->
                        Decode.succeed Gone

                    _ ->
                        Decode.fail ("no such release: " ++ how)
            )


{-| The slot is free again. A release for an exchange that holds no slot
breaks law 4, so it is `Nothing`, never ignored.
-}
released : RequestId -> Pool -> Maybe Pool
released id (Pool p) =
    let
        key =
            Request.idToString id
    in
    if Set.member key p.held then
        Just (Pool { p | held = Set.remove key p.held })

    else
        Nothing



-- PRE-FLIGHT


{-| What probing a final name proved.

Final names are made only by `link(2)`, which never replaces one, and the
server never removes one. So `Present` is stationary for the one question
it answers, "may this upload commit" (L-spec Theorem 15): once seen, it
stays true. `NotSeen` is not: another upload can commit between the probe
and our link. A witness can therefore justify a refusal but never grant a
commit; after `NotSeen`, the link still decides.

-}
type Witness
    = Present
    | NotSeen


{-| The final name a route must probe before it lets a body in: uploads
and ingests are create-only, so if the name exists the answer is 409
whatever the body holds.
-}
preflight : Layout -> Route -> Maybe Final
preflight layout route =
    case route of
        Route.Upload id ->
            Just (Layout.blob layout id)

        Route.Ingest id ->
            Just (Layout.ingested layout id)

        Route.Health ->
            Nothing

        Route.Item _ ->
            Nothing

        Route.Echo ->
            Nothing

        Route.Delayed _ ->
            Nothing

        Route.Stats ->
            Nothing

        Route.Blob _ ->
            Nothing


{-| A probe's outcome as a witness. ENOENT is the one failure that means
"not there"; any other is a host error, not evidence of absence.
-}
witness : Result HostError a -> Result HostError Witness
witness outcome =
    case outcome of
        Ok _ ->
            Ok Present

        Err error ->
            case Io.classify error of
                Io.NoEntry ->
                    Ok NotSeen

                _ ->
                    Err error


refusal : Witness -> Maybe Response
refusal seen =
    case seen of
        Present ->
            Just (Response.problem Response.Conflict "this id has already been committed" [])

        NotSeen ->
            Nothing



-- TRACES


{-| One exchange's work, sealed: storage steps built by `Plan`, then what
the bridge does when they are done.
-}
type KernelTrace
    = KernelTrace RequestId Plan.Program Ending


type Ending
    = Yield
    | Respond Answer
    | Streamed


{-| The program pauses; Elm reads the outcomes and writes the next trace.
-}
yieldAfter : ( Plan.Program, Plan.Suspended state ) -> Exchange body -> ( KernelTrace, Plan.Suspended state )
yieldAfter ( program, suspended ) (Exchange id) =
    ( KernelTrace id program Yield, suspended )


respondAfter : Plan.Program -> Answer -> KernelTrace
respondAfter program ((Answer id _ _) as given) =
    KernelTrace id program (Respond given)


{-| A step wrote the answer itself, a download. Only a drained exchange
streams, so the connection stays open.
-}
streamedAfter : Plan.Program -> Exchange Drained -> KernelTrace
streamedAfter program (Exchange id) =
    KernelTrace id program Streamed


encode : KernelTrace -> Encode.Value
encode (KernelTrace id program ending) =
    Encode.object
        [ ( "do", Encode.string "trace" )
        , ( "program", Tx.encodeBegin (Request.idToString id) (Plan.steps program) (Plan.continuation program) )
        , ( "ending", encodeEnding ending )
        ]


encodeEnding : Ending -> Encode.Value
encodeEnding ending =
    case ending of
        Yield ->
            Encode.object [ ( "kind", Encode.string "yield" ) ]

        Streamed ->
            Encode.object [ ( "kind", Encode.string "streamed" ) ]

        Respond (Answer _ response fate) ->
            let
                wire =
                    Response.toWire Response.KeepAlive response
            in
            Encode.object
                [ ( "kind", Encode.string "respond" )
                , ( "status", Encode.int wire.status )
                , ( "headers"
                  , Encode.list Encode.string <|
                        case fate of
                            KeepAlive ->
                                wire.headers

                            Dissipate _ ->
                                wire.headers ++ [ "connection", "close" ]

                            Sever ->
                                wire.headers ++ [ "connection", "close" ]
                  )
                , ( "body", Encode.string wire.body )
                , ( "fate", encodeFate fate )
                ]


encodeFate : Fate -> Encode.Value
encodeFate fate =
    case fate of
        KeepAlive ->
            Encode.object [ ( "kind", Encode.string "keepAlive" ) ]

        Sever ->
            Encode.object [ ( "kind", Encode.string "sever" ) ]

        Dissipate profile ->
            Encode.object
                [ ( "kind", Encode.string "dissipate" )
                , ( "deadlineMs", Encode.int profile.deadlineMs )
                , ( "budgetBytes", Encode.int profile.budgetBytes )
                , ( "whenSpent"
                  , Encode.string <|
                        case profile.whenSpent of
                            Cut ->
                                "cut"

                            Hold ->
                                "hold"
                  )
                ]
