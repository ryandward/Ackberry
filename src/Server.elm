module Server exposing
    ( Answer(..)
    , Client(..)
    , Clock
    , Config
    , Conn
    , ConnPhase(..)
    , Direction(..)
    , Effect(..)
    , Ending(..)
    , Job
    , Ledger
    , Lingered
    , Meter
    , Model
    , Msg(..)
    , Owner(..)
    , Phase(..)
    , Rate
    , Sweep(..)
    , Work(..)
    , eventDecoder
    , init
    , maxUpstreamBytes
    , ownerFromKey
    , ownerKey
    , step
    )

{-| The server as a pure state machine, from the first byte of a connection
to its last.

The bridge owns nothing but sockets and file descriptors. It reports what
happened (a connection was accepted, a head arrived, the peer closed, a
step's outcome) with both of the host's clocks, and carries out what Elm
sends: write these bytes, read the next head, close, dissipate, run this
transaction.

A connection is awaiting a head, carrying one exchange, or closing. Elm
reads the next head only after the current response is out, so a
connection never has two exchanges, and decides after every response
whether the connection persists, is dissipated (its body was never read),
or is closed.

A request that touches the host becomes one transaction (see `Tx`), run by
the bridge and held by Elm. Elm keeps three kinds of authority over it:

  - **Abort.** The client leaving, the deadline, and the rate meter all end
    the same way: `Abort tx`. The bridge severs whatever interruptible step
    is in flight and unwinds the cleanups the transaction registered.
  - **Pause.** A transaction can stop and hand control back with its
    cleanups still armed; Elm extends it or lets it finish.
  - **The meter.** Streaming steps report bytes moved. A body that arrives
    slower than its rate is aborted with 408 (slowloris), and a response
    body that leaves slower than its rate is aborted (a slow reader holding
    an fd).

Time is data. Every deadline (a head's, an idle connection's, a job's, a
meter's) is a monotonic instant compared with the clock the bridge reports;
Elm asks the bridge for one alarm at a time, every `tickMs` while anything
has a deadline and otherwise at the janitor's next sweep, and none once the
server has stopped and nothing is left. The wall clock is used only for the
Date header and, by the janitor, against file mtimes.

The server opens only once its layout exists: `init` begins a transaction
that creates the parts and blob directories, and `Listen` goes out only
when it settles cleanly. See THE JANITOR for the parts crashed processes
leave behind.

-}

import Commit
import Dict exposing (Dict)
import Domain exposing (ItemId)
import Http.Chunked as Chunked
import Http.Date as Date
import Http.Head as Head
import Http.Request as Request exposing (ConnId, Request, RequestId)
import Http.Response as Response exposing (Response)
import Http.Route as Route exposing (Route)
import Io exposing (HostError)
import Json.Decode as Decode
import Json.Encode as Encode
import Kernel
import Layout exposing (Layout, Part)
import Plan
import Tx
import Verdict exposing (Fault(..), Verdict)


type alias Rate =
    { bytes : Int
    , perMs : Int
    }


type alias Config =
    { dataDir : String
    , upstream : String
    , deadlineMs : Int
    , transferDeadlineMs : Int
    , transferRate : Rate
    , deliveryRate : Rate
    , jsonLimit : Int
    , uploadLimit : Int
    , progressEveryMs : Int
    , staleAfterMs : Int
    , sweepEveryMs : Int
    , head : Head.Limits
    , headDeadlineMs : Int
    , idleMs : Int
    , tickMs : Int
    , linger : { deadlineMs : Int, budgetBytes : Int, maxSockets : Int }
    , chunked : Chunked.Limits
    }


{-| The host's two clocks, read by the bridge when it reported an event.
`mono` (milliseconds of `performance.now()`) never jumps and times every
deadline; `wall` (the epoch clock) can, and dates responses only.
-}
type alias Clock =
    { mono : Int
    , wall : Int
    }


type alias Model =
    { config : Config
    , layout : Layout
    , phase : Phase
    , sweep : Sweep
    , clock : Clock
    , nextSweepAt : Int
    , alarm : Maybe Int
    , jobs : Dict String Job
    , conns : Dict String Conn
    , pool : Kernel.Pool
    , ledger : Ledger
    }


{-| `Stopped` is final: the layout could not be prepared, or the bridge's
server closed. Exchanges in flight finish and their connections close;
nothing new is read or swept.
-}
type Phase
    = Booting
    | Serving
    | Stopped


{-| The janitor's one transaction, if it has one. `Preparing` is the boot
transaction; a sweep is `Surveying` until the listing comes back, then
`Discarding` the parts it judged stale.
-}
type Sweep
    = Preparing
    | Surveying (Plan.Suspended Plan.Fresh)
    | Discarding (List Part)
    | Idle


{-| Who a transaction or a fault belongs to: a client's exchange, the
server's janitor, or a connection before or after any exchange on it.
-}
type Owner
    = Exchange RequestId
    | Janitor
    | Socket ConnId


{-| `served` counts the exchanges a connection has carried, and names the
next one. `ended` is true once the peer has sent FIN: nothing more will
arrive, so the connection cannot persist past its current exchange.
-}
type alias Conn =
    { phase : ConnPhase
    , served : Int
    , ended : Bool
    }


{-| The first head must arrive within `headDeadlineMs` of the connection,
each later one within `idleMs` of the last response. `lingering` is true
while a dissipating connection holds a slot in the pool.
-}
type ConnPhase
    = AwaitingHead { deadline : Int, idle : Bool }
    | Exchanging RequestId
    | Closing { lingering : Bool }


type alias Job =
    { client : Client
    , conn : ConnId
    , work : Work
    , body : BodyState
    , persistence : Head.Persistence
    , meter : Maybe Meter
    , deadline : Int
    }


type Client
    = Waiting
    | Answered
    | Gone


type BodyState
    = NoBody
    | Unread
    | Drained


type Direction
    = Inbound
    | Outbound


{-| A rate requirement on the transaction's streaming step. `running`
becomes true when the bridge reports the step has started, so time spent
opening or fsyncing never counts against the client. At `checkAt` the step
must have moved `required` bytes in total.
-}
type alias Meter =
    { direction : Direction
    , rate : Rate
    , moved : Int
    , running : Bool
    , checkAt : Int
    , required : Int
    }


{-| What the transaction's next report means. A download knows whether its
connection closes after it, because its head, written before the first
byte, says so.
-}
type Work
    = Echoing
    | ReceivingJson ItemId (Plan.Suspended Plan.Fresh)
    | Committing Commit.Spec
    | Fetching ItemId
    | OpeningBlob ItemId (Plan.Suspended Plan.Reading)
    | Delivering { closes : Bool }
    | Finishing


{-| Conservation law, true after every step:

    received == responded + abandoned + jobs whose client is Waiting

-}
type alias Ledger =
    { received : Int
    , responded : Int
    , abandoned : Int
    , timedOut : Int
    , tooSlow : Int
    , aborted : Int
    , faults : Int
    , orphans : Int
    , swept : Int
    , accepted : Int
    , lingered : Lingered
    }


{-| How connections whose body was never read ended: the client hung up
before any bound was crossed, or the deadline or the byte budget cut it,
or the pool was full and the close was immediate. And how many bytes the
bridge read and dropped for them.
-}
type alias Lingered =
    { hungUp : Int
    , deadlinePassed : Int
    , budgetSpent : Int
    , poolFull : Int
    , bytesDiscarded : Int
    }


{-| Every event carries the clock at the moment the bridge saw it, so an
event and its time are written by the same producer at once (L-spec
Corollary 16.2). `Woke` is the alarm Elm set. `Garbled` is an event that
does not decode, which no state may absorb silently.
-}
type Msg
    = Accepted ConnId Clock
    | HeadArrived ConnId String Clock
    | HeadOverflow ConnId Clock
    | PeerEnded ConnId Clock
    | ConnClosed ConnId Ending Clock
    | Reported Owner Decode.Value Clock
    | Progressed RequestId Int Bool Clock
    | Woke Clock
    | ServerClosed Clock
    | Garbled String


{-| How a connection's fd came to be closed: by the peer while the server
still held it, as the server asked, or at the end of a dissipation (with
the bytes it dropped).
-}
type Ending
    = PeerLeft
    | AsAsked
    | Dissipated Kernel.Release Int


{-| Each becomes one entry in the single ordered instruction array `Main`
sends per update.
-}
type Effect
    = Send ConnId String
    | AwaitHead ConnId Int
    | CloseConn ConnId
    | Dissipate ConnId Kernel.Dissipation
    | Begin Owner Plan.Program
    | Extend Owner Plan.Program
    | Abort RequestId
    | Report Owner Fault
    | Note RequestId ConnId String String
    | Alarm (Maybe Int)
    | Listen
    | Halt String


{-| A transaction's or a fault's name on the wire. A request's is
`<connection uuid>.<n>`; "janitor" and "socket <uuid>" can be neither, so
`ownerFromKey` inverts `ownerKey`.
-}
ownerKey : Owner -> String
ownerKey owner =
    case owner of
        Exchange id ->
            Request.idToString id

        Janitor ->
            "janitor"

        Socket conn ->
            "socket " ++ Request.connIdToString conn


ownerFromKey : String -> Owner
ownerFromKey name =
    if name == "janitor" then
        Janitor

    else if String.startsWith "socket " name then
        Socket (Request.connIdFromString (String.dropLeft 7 name))

    else
        Exchange (Request.idFromString name)


eventDecoder : Decode.Decoder Msg
eventDecoder =
    let
        clock =
            Decode.map2 Clock
                (Decode.field "mono" (Decode.map floor Decode.float))
                (Decode.field "wall" Decode.int)

        conn =
            Decode.field "conn" (Decode.map Request.connIdFromString Decode.string)

        ending =
            Decode.map2 Tuple.pair (Decode.field "how" Decode.string) (Decode.field "discarded" Decode.int)
                |> Decode.andThen
                    (\( how, bytes ) ->
                        case how of
                            "peer" ->
                                Decode.succeed PeerLeft

                            "asked" ->
                                Decode.succeed AsAsked

                            _ ->
                                case Kernel.releaseFromString how of
                                    Just release ->
                                        Decode.succeed (Dissipated release bytes)

                                    Nothing ->
                                        Decode.fail ("no such ending: " ++ how)
                    )
    in
    Decode.field "event" Decode.string
        |> Decode.andThen
            (\event ->
                case event of
                    "accepted" ->
                        Decode.map2 Accepted conn clock

                    "head" ->
                        Decode.map3 HeadArrived conn (Decode.field "head" Decode.string) clock

                    "overflow" ->
                        Decode.map2 HeadOverflow conn clock

                    "ended" ->
                        Decode.map2 PeerEnded conn clock

                    "closed" ->
                        Decode.map3 ConnClosed conn ending clock

                    "report" ->
                        Decode.map3 Reported (Decode.field "tx" (Decode.map ownerFromKey Decode.string)) (Decode.field "report" Decode.value) clock

                    "progress" ->
                        Decode.map4 Progressed
                            (Decode.field "tx" (Decode.map Request.idFromString Decode.string))
                            (Decode.field "bytes" Decode.int)
                            (Decode.field "running" Decode.bool)
                            clock

                    "alarm" ->
                        Decode.map Woke clock

                    "serverClosed" ->
                        Decode.map ServerClosed clock

                    _ ->
                        Decode.fail ("no such event: " ++ event)
            )


maxUpstreamBytes : Int
maxUpstreamBytes =
    65536


init : Config -> ( Model, List Effect )
init config =
    let
        model =
            { config = config
            , layout = Layout.fromDataDir config.dataDir
            , phase = Booting
            , sweep = Preparing
            , clock = { mono = 0, wall = 0 }
            , nextSweepAt = 0
            , alarm = Nothing
            , jobs = Dict.empty
            , conns = Dict.empty
            , pool = Kernel.pool config.linger.maxSockets
            , ledger =
                { received = 0
                , responded = 0
                , abandoned = 0
                , timedOut = 0
                , tooSlow = 0
                , aborted = 0
                , faults = 0
                , orphans = 0
                , swept = 0
                , accepted = 0
                , lingered = { hungUp = 0, deadlinePassed = 0, budgetSpent = 0, poolFull = 0, bytesDiscarded = 0 }
                }
            }
    in
    case misconfigured config of
        Just why ->
            ( { model | phase = Stopped, sweep = Idle }, [ Halt why ] )

        Nothing ->
            ( model, [ Begin Janitor (Plan.fresh |> Plan.prepare model.layout |> Plan.end) ] )


misconfigured : Config -> Maybe String
misconfigured config =
    if config.staleAfterMs <= config.transferDeadlineMs then
        Just "staleAfterMs must exceed transferDeadlineMs, or the janitor could take a part from a transaction that can still commit it"

    else if config.sweepEveryMs <= 0 || config.tickMs <= 0 || config.headDeadlineMs <= 0 || config.idleMs <= 0 then
        Just "sweepEveryMs, tickMs, headDeadlineMs and idleMs must be positive"

    else if config.head.maxHeadBytes <= 0 || config.head.maxTarget <= 0 || config.head.maxFields <= 0 then
        Just "the head limits must be positive"

    else if config.linger.deadlineMs < 0 || config.linger.budgetBytes < 0 || config.linger.maxSockets < 0 then
        Just "the linger bounds must not be negative"

    else
        Nothing


step : Msg -> Model -> ( Model, List Effect )
step msg model =
    let
        clocked =
            case clockOf msg of
                Just clock ->
                    { model | clock = clock }

                Nothing ->
                    model
    in
    rearm (handle msg clocked)


clockOf : Msg -> Maybe Clock
clockOf msg =
    case msg of
        Accepted _ clock ->
            Just clock

        HeadArrived _ _ clock ->
            Just clock

        HeadOverflow _ clock ->
            Just clock

        PeerEnded _ clock ->
            Just clock

        ConnClosed _ _ clock ->
            Just clock

        Reported _ _ clock ->
            Just clock

        Progressed _ _ _ clock ->
            Just clock

        Woke clock ->
            Just clock

        ServerClosed clock ->
            Just clock

        Garbled _ ->
            Nothing


handle : Msg -> Model -> ( Model, List Effect )
handle msg model =
    case msg of
        Accepted conn _ ->
            let
                counted =
                    count (\l -> { l | accepted = l.accepted + 1 }) model
            in
            case model.phase of
                Serving ->
                    ( putConn conn { phase = AwaitingHead { deadline = model.clock.mono + model.config.headDeadlineMs, idle = False }, served = 0, ended = False } counted
                    , [ AwaitHead conn model.config.head.maxHeadBytes ]
                    )

                _ ->
                    -- The listener is closing: nothing new is served.
                    ( putConn conn { phase = Closing { lingering = False }, served = 0, ended = False } counted, [ CloseConn conn ] )

        HeadArrived conn text _ ->
            withConn conn model <|
                \c ->
                    case c.phase of
                        AwaitingHead _ ->
                            arrive conn c text model

                        Closing _ ->
                            -- Read under an instruction sent before Elm chose
                            -- to close: the two crossed, and the close stands.
                            ( model, [] )

                        Exchanging _ ->
                            report (Socket conn) [ Anomaly "a head arrived while the connection carried an exchange" ] model

        HeadOverflow conn _ ->
            withConn conn model <|
                \c ->
                    case c.phase of
                        AwaitingHead _ ->
                            refuseHead conn
                                c
                                (Response.problem Response.HeaderFieldsTooLarge
                                    ("the request head did not end within " ++ String.fromInt model.config.head.maxHeadBytes ++ " bytes")
                                    []
                                )
                                model

                        Closing _ ->
                            ( model, [] )

                        Exchanging _ ->
                            report (Socket conn) [ Anomaly "a head overflowed while the connection carried an exchange" ] model

        PeerEnded conn _ ->
            withConn conn model <|
                \c ->
                    case c.phase of
                        AwaitingHead _ ->
                            -- Nothing more will arrive, so no head will complete.
                            ( putConn conn { c | ended = True, phase = Closing { lingering = False } } model, [ CloseConn conn ] )

                        Exchanging id ->
                            -- A client that ends its side mid-exchange has left:
                            -- its work is aborted, as if it had closed.
                            clientGone id (putConn conn { c | ended = True } model)

                        Closing _ ->
                            ( putConn conn { c | ended = True } model, [] )

        ConnClosed conn ending _ ->
            withConn conn model <|
                \c -> closed conn c ending model

        Progressed id bytes running clock ->
            withJob id model <|
                \job ->
                    case job.meter of
                        Nothing ->
                            ( model, [] )

                        Just meter ->
                            if running && not meter.running then
                                -- The metered step just started: start its clock.
                                ( putJob id
                                    { job
                                        | meter =
                                            Just
                                                { meter
                                                    | running = True
                                                    , moved = bytes
                                                    , checkAt = clock.mono + meter.rate.perMs
                                                    , required = bytes + meter.rate.bytes
                                                }
                                    }
                                    model
                                , []
                                )

                            else
                                ( putJob id { job | meter = Just { meter | running = running, moved = bytes } } model, [] )

        Woke _ ->
            let
                woken =
                    { model | alarm = Nothing }

                ( afterJobs, jobEffects ) =
                    Dict.foldl
                        (\owner _ ( m, effects ) ->
                            let
                                ( next, more ) =
                                    expire (Request.idFromString owner) m
                            in
                            ( next, effects ++ more )
                        )
                        ( woken, [] )
                        woken.jobs

                ( afterConns, connEffects ) =
                    expireConns afterJobs

                ( afterSweep, sweepEffects ) =
                    sweepIfDue afterConns
            in
            ( afterSweep, jobEffects ++ connEffects ++ sweepEffects )

        ServerClosed _ ->
            -- Exchanges in flight finish; connections waiting for a head
            -- are closed now rather than at their deadlines.
            Dict.foldl
                (\k c ( m, effects ) ->
                    case c.phase of
                        AwaitingHead _ ->
                            ( putConn (Request.connIdFromString k) { c | phase = Closing { lingering = False } } m
                            , effects ++ [ CloseConn (Request.connIdFromString k) ]
                            )

                        _ ->
                            ( m, effects )
                )
                ( { model | phase = Stopped }, [] )
                model.conns

        Garbled detail ->
            report Janitor [ Anomaly ("an event from the bridge that does not decode: " ++ detail) ] model

        Reported Janitor value _ ->
            case Decode.decodeValue Tx.reportDecoder value of
                Ok txReport ->
                    janitorReported txReport model

                Err error ->
                    janitorBroke ("a report that does not decode: " ++ Decode.errorToString error) model

        Reported (Socket conn) _ _ ->
            report (Socket conn) [ Anomaly "a transaction report for a connection, which runs none" ] model

        Reported (Exchange id) value _ ->
            case Dict.get (key id) model.jobs of
                Nothing ->
                    report (Exchange id) [ Anomaly "a report for a transaction nobody holds" ] model

                Just job ->
                    case Decode.decodeValue Tx.reportDecoder value of
                        Ok txReport ->
                            react id job txReport model

                        Err error ->
                            -- Whether it settled is unknowable; end the job
                            -- loudly and tell the bridge to unwind if it can.
                            let
                                ( concluded, effects ) =
                                    conclude id job Nothing [ Anomaly ("a report that does not decode: " ++ Decode.errorToString error) ] model
                            in
                            ( concluded, effects ++ [ Abort id ] )


{-| One alarm at a time: every `tickMs` while anything has a deadline,
otherwise at the janitor's next sweep, and none once nothing is left. An
alarm already set earlier than needed is left alone; it fires, and the
next one is set then.
-}
rearm : ( Model, List Effect ) -> ( Model, List Effect )
rearm ( model, effects ) =
    let
        awaiting c =
            case c.phase of
                AwaitingHead _ ->
                    True

                Exchanging _ ->
                    False

                Closing _ ->
                    False

        janitorIdle =
            case model.sweep of
                Idle ->
                    True

                _ ->
                    False

        wanted =
            if not (Dict.isEmpty model.jobs) || List.any awaiting (Dict.values model.conns) then
                Just (model.clock.mono + model.config.tickMs)

            else if model.phase == Serving && janitorIdle then
                Just model.nextSweepAt

            else
                Nothing

        arm at =
            ( { model | alarm = Just at }, effects ++ [ Alarm (Just (max 0 (at - model.clock.mono))) ] )
    in
    case ( wanted, model.alarm ) of
        ( Nothing, Nothing ) ->
            ( model, effects )

        ( Nothing, Just _ ) ->
            ( { model | alarm = Nothing }, effects ++ [ Alarm Nothing ] )

        ( Just at, Just current ) ->
            if current <= at then
                ( model, effects )

            else
                arm at

        ( Just at, Nothing ) ->
            arm at


sweepIfDue : Model -> ( Model, List Effect )
sweepIfDue model =
    case ( model.phase, model.sweep ) of
        ( Serving, Idle ) ->
            if model.clock.mono >= model.nextSweepAt then
                let
                    ( program, suspended ) =
                        Plan.fresh |> Plan.survey model.layout |> Plan.suspend
                in
                ( { model | sweep = Surveying suspended }, [ Begin Janitor program ] )

            else
                ( model, [] )

        _ ->
            ( model, [] )


{-| The janitor at rest, its next sweep `sweepEveryMs` from now.
-}
idle : Model -> Model
idle model =
    { model | sweep = Idle, nextSweepAt = model.clock.mono + model.config.sweepEveryMs }



-- CONNECTIONS


{-| A head is a request: it is counted as received the moment it arrives,
answered even if it is refused.
-}
arrive : ConnId -> Conn -> String -> Model -> ( Model, List Effect )
arrive conn c text model =
    let
        id =
            Request.idWithin conn c.served

        counted =
            count (\l -> { l | received = l.received + 1 })
                (putConn conn { c | phase = Exchanging id, served = c.served + 1 } model)
    in
    case Head.parse model.config.head text of
        Err problem ->
            answerNow conn { body = Unread, persistence = Head.Close } (headRefusal problem) counted

        Ok head ->
            let
                request =
                    Head.toRequest head

                ( begun, effects ) =
                    -- Routes see the model before this request is counted,
                    -- so /stats reports a ledger that balances.
                    begin id conn { body = declaredBody request, persistence = head.persistence } (route model id conn request) counted
            in
            ( begun, Note id conn (Request.methodToString head.method) head.target :: effects )


{-| A head that never became a request, counted as one so the ledger
balances: received, answered, closed.
-}
refuseHead : ConnId -> Conn -> Response -> Model -> ( Model, List Effect )
refuseHead conn c response model =
    answerNow conn
        { body = Unread, persistence = Head.Close }
        response
        (count (\l -> { l | received = l.received + 1 }) (putConn conn { c | phase = Exchanging (Request.idWithin conn c.served), served = c.served + 1 } model))


{-| The fd is closed. An exchange still waiting on it has lost its client;
a dissipation gives back its slot and is counted.
-}
closed : ConnId -> Conn -> Ending -> Model -> ( Model, List Effect )
closed conn c ending model =
    let
        forgotten =
            { model | conns = Dict.remove (connKey conn) model.conns }

        ( freed, faults ) =
            case c.phase of
                Closing { lingering } ->
                    if lingering then
                        case Kernel.released conn forgotten.pool of
                            Just p ->
                                ( { forgotten | pool = p }, [] )

                            Nothing ->
                                ( forgotten, [ Anomaly "a dissipated connection that held no slot" ] )

                    else
                        ( forgotten, [] )

                AwaitingHead _ ->
                    ( forgotten, [] )

                Exchanging _ ->
                    ( forgotten, [] )

        tallied =
            case ending of
                Dissipated release bytes ->
                    count (\l -> { l | lingered = tally release bytes l.lingered }) freed

                PeerLeft ->
                    freed

                AsAsked ->
                    freed

        ( reported, reports ) =
            report (Socket conn) faults tallied
    in
    case c.phase of
        Exchanging id ->
            let
                ( gone, effects ) =
                    clientGone id reported
            in
            ( gone, reports ++ effects )

        AwaitingHead _ ->
            ( reported, reports )

        Closing _ ->
            ( reported, reports )


clientGone : RequestId -> Model -> ( Model, List Effect )
clientGone id model =
    withJob id model <|
        \job ->
            case job.client of
                Waiting ->
                    ( model
                        |> putJob id { job | client = Gone }
                        |> count (\l -> { l | abandoned = l.abandoned + 1, aborted = l.aborted + 1 })
                    , [ Abort id ]
                    )

                Answered ->
                    ( model, [] )

                Gone ->
                    ( model, [] )


{-| Connections whose head is late: the first head of a connection is
answered 408, an idle connection between requests is closed without a word.
-}
expireConns : Model -> ( Model, List Effect )
expireConns model =
    Dict.foldl
        (\k c ( m, effects ) ->
            let
                conn =
                    Request.connIdFromString k
            in
            case c.phase of
                AwaitingHead wait ->
                    if m.clock.mono < wait.deadline then
                        ( m, effects )

                    else if wait.idle then
                        ( putConn conn { c | phase = Closing { lingering = False } } m, effects ++ [ CloseConn conn ] )

                    else
                        let
                            ( refused, more ) =
                                refuseHead conn
                                    c
                                    (Response.problem Response.RequestTimeout
                                        ("the request head did not arrive within " ++ String.fromInt m.config.headDeadlineMs ++ " ms")
                                        []
                                    )
                                    (count (\l -> { l | timedOut = l.timedOut + 1 }) m)
                        in
                        ( refused, effects ++ more )

                Exchanging _ ->
                    ( m, effects )

                Closing _ ->
                    ( m, effects )
        )
        ( model, [] )
        model.conns


headRefusal : Head.Problem -> Response
headRefusal problem =
    let
        bad detail =
            Response.problem Response.BadRequest detail []
    in
    case problem of
        Head.MalformedRequestLine ->
            bad "the request line is not HTTP/1.x"

        Head.TargetTooLong ->
            Response.problem Response.UriTooLong "the request target is too long" []

        Head.UnsupportedVersion ->
            Response.problem Response.VersionNotSupported "only HTTP/1.x is spoken here" []

        Head.UnsupportedMethod method ->
            Response.problem Response.NotImplemented ("method " ++ method ++ " is not implemented") [ ( "method", Encode.string method ) ]

        Head.HeadTooLarge ->
            Response.problem Response.HeaderFieldsTooLarge "the request head is too large" []

        Head.MalformedField ->
            bad "a header field is malformed"

        Head.MissingHost ->
            bad "an HTTP/1.1 request needs a Host"

        Head.MultipleHosts ->
            bad "a request may name only one Host"

        Head.InvalidHost ->
            bad "the Host or the target's authority is not a host"

        Head.HostMismatch ->
            bad "the target's authority and the Host name different hosts"

        Head.InvalidContentLength ->
            bad "Content-Length is not one non-negative integer"

        Head.ContentLengthTooLarge ->
            Response.problem Response.PayloadTooLarge "the declared body is longer than this server can count" []

        Head.AmbiguousLength ->
            bad "Content-Length beside Transfer-Encoding leaves the body's length ambiguous"

        Head.FaultyFraming ->
            bad "the transfer codings do not end in a single chunked"

        Head.UnsupportedCoding ->
            Response.problem Response.NotImplemented "chunked is the only transfer coding this server reads" []

        Head.UnsupportedExpectation ->
            Response.problem Response.ExpectationFailed "100-continue is the only expectation this server meets" []



-- RESPONDING


{-| What decides a connection's fate after a response.
-}
type alias Disposition =
    { body : BodyState
    , persistence : Head.Persistence
    }


{-| A connection persists only if its body was read to the end (or there
was none), the client asked to keep it, the client has not closed its
side, and the server is still serving.
-}
persists : Model -> ConnId -> Disposition -> Bool
persists model conn disposition =
    disposition.body
        /= Unread
        && disposition.persistence
        == Head.KeepAlive
        && model.phase
        == Serving
        && (Dict.get (connKey conn) model.conns |> Maybe.map (not << .ended) |> Maybe.withDefault False)


{-| The one way a response leaves: serialized here, written on its
connection, then the connection's fate. A connection that is gone has
nobody to answer.
-}
respond : ConnId -> Disposition -> Response -> Model -> ( Model, List Effect )
respond conn disposition response model =
    case Dict.get (connKey conn) model.conns of
        Nothing ->
            ( model, [] )

        Just c ->
            let
                date =
                    Date.fromMillis model.clock.wall
            in
            if persists model conn disposition then
                ( putConn conn { c | phase = awaitingNext model } model
                , [ Send conn (Response.serialize date False response), AwaitHead conn model.config.head.maxHeadBytes ]
                )

            else
                let
                    ( ending, next ) =
                        closeConn conn c disposition.body model
                in
                ( next, Send conn (Response.serialize date True response) :: ending )


awaitingNext : Model -> ConnPhase
awaitingNext model =
    AwaitingHead { deadline = model.clock.mono + model.config.idleMs, idle = True }


{-| A connection whose body was never read is dissipated if the pool has a
slot and closed at once if not; any other is simply closed.
-}
closeConn : ConnId -> Conn -> BodyState -> Model -> ( List Effect, Model )
closeConn conn c body model =
    case body of
        Unread ->
            case Kernel.dissipate { deadlineMs = model.config.linger.deadlineMs, budgetBytes = model.config.linger.budgetBytes } conn model.pool of
                ( Kernel.Dissipating profile, taken ) ->
                    ( [ Dissipate conn profile ], putConn conn { c | phase = Closing { lingering = True } } { model | pool = taken } )

                ( Kernel.Severed, _ ) ->
                    ( [ CloseConn conn ]
                    , putConn conn { c | phase = Closing { lingering = False } } model
                        |> count (\l -> { l | lingered = poolWasFull l.lingered })
                    )

        NoBody ->
            ( [ CloseConn conn ], putConn conn { c | phase = Closing { lingering = False } } model )

        Drained ->
            ( [ CloseConn conn ], putConn conn { c | phase = Closing { lingering = False } } model )


poolWasFull : Lingered -> Lingered
poolWasFull lingered =
    { lingered | poolFull = lingered.poolFull + 1 }


tally : Kernel.Release -> Int -> Lingered -> Lingered
tally how bytes lingered =
    let
        counted =
            { lingered | bytesDiscarded = lingered.bytesDiscarded + bytes }
    in
    case how of
        Kernel.HungUp ->
            { counted | hungUp = counted.hungUp + 1 }

        Kernel.DeadlinePassed ->
            { counted | deadlinePassed = counted.deadlinePassed + 1 }

        Kernel.BudgetSpent ->
            { counted | budgetSpent = counted.budgetSpent + 1 }


dispositionOf : Job -> Disposition
dispositionOf job =
    { body = job.body, persistence = job.persistence }


{-| A download wrote its own head and bytes. If it finished and its head
promised to keep the connection, the next head is read; otherwise the
connection closes, which also ends a stream that was cut short.
-}
afterStream : ConnId -> { closes : Bool } -> Bool -> ( Model, List Effect ) -> ( Model, List Effect )
afterStream conn streamed finished ( model, effects ) =
    case Dict.get (connKey conn) model.conns of
        Just c ->
            case c.phase of
                Exchanging _ ->
                    if finished && not streamed.closes then
                        ( putConn conn { c | phase = awaitingNext model } model, effects ++ [ AwaitHead conn model.config.head.maxHeadBytes ] )

                    else
                        ( putConn conn { c | phase = Closing { lingering = False } } model, effects ++ [ CloseConn conn ] )

                AwaitingHead _ ->
                    ( model, effects )

                Closing _ ->
                    ( model, effects )

        Nothing ->
            ( model, effects )


{-| The clock reached this job: its deadline, then its meter.
-}
expire : RequestId -> Model -> ( Model, List Effect )
expire id model =
    withJob id model <|
        \job ->
            if job.client == Waiting && model.clock.mono >= job.deadline then
                let
                    response =
                        if receiving job then
                            Response.problem Response.RequestTimeout
                                ("the request body did not arrive within " ++ String.fromInt model.config.transferDeadlineMs ++ " ms")
                                []

                        else
                            Response.problem Response.GatewayTimeout
                                ("no answer within " ++ String.fromInt model.config.deadlineMs ++ " ms")
                                []

                    ( answered, effects ) =
                        model
                            |> putJob id { job | client = Answered }
                            |> count (\l -> { l | responded = l.responded + 1, timedOut = l.timedOut + 1, aborted = l.aborted + 1 })
                            |> respond job.conn (dispositionOf job) response
                in
                ( answered, abortFirst id effects )

            else
                case job.meter of
                    Just meter ->
                        -- A count may trail the bytes by up to progressEveryMs,
                        -- so a window is judged only once every byte that arrived
                        -- in it must have been reported. Judging earlier reads a
                        -- stale count as the present (L-spec Theorem 14).
                        if not meter.running || model.clock.mono < meter.checkAt + model.config.progressEveryMs then
                            ( model, [] )

                        else if meter.moved >= meter.required then
                            ( putJob id
                                { job
                                    | meter =
                                        Just
                                            { meter
                                                | checkAt = model.clock.mono + meter.rate.perMs
                                                , required = meter.moved + meter.rate.bytes
                                            }
                                }
                                model
                            , []
                            )

                        else
                            tooSlow id job meter model

                    Nothing ->
                        ( model, [] )


receiving : Job -> Bool
receiving job =
    case job.meter of
        Just meter ->
            meter.running && meter.direction == Inbound

        Nothing ->
            False


tooSlow : RequestId -> Job -> Meter -> Model -> ( Model, List Effect )
tooSlow id job meter model =
    let
        slow =
            count (\l -> { l | tooSlow = l.tooSlow + 1, aborted = l.aborted + 1 }) model
    in
    case ( meter.direction, job.client ) of
        ( Inbound, Waiting ) ->
            -- The 408 goes out first, then the abort severs the body step,
            -- which lets go of the socket before the dissipation takes it.
            let
                ( answered, effects ) =
                    slow
                        |> putJob id { job | client = Answered, meter = Just { meter | running = False } }
                        |> count (\l -> { l | responded = l.responded + 1 })
                        |> respond job.conn
                            (dispositionOf job)
                            (Response.problem Response.RequestTimeout
                                ("the body arrived slower than " ++ String.fromInt meter.rate.bytes ++ " bytes per " ++ String.fromInt meter.rate.perMs ++ " ms")
                                [ ( "bytesReceived", Encode.int meter.moved ) ]
                            )
            in
            ( answered, abortFirst id effects )

        ( Inbound, Answered ) ->
            ( model, [] )

        ( Inbound, Gone ) ->
            ( model, [] )

        ( Outbound, _ ) ->
            -- The status line is long gone; all that is left is the fd.
            ( putJob id { job | meter = Just { meter | running = False } } slow, [ Abort id ] )


{-| The answer's bytes, then the abort that frees the socket from the
body step, then the connection's fate: a dissipation must not start
reading while a body step still is.
-}
abortFirst : RequestId -> List Effect -> List Effect
abortFirst id effects =
    case effects of
        (Send conn text) :: fate ->
            Send conn text :: Abort id :: fate

        _ ->
            effects ++ [ Abort id ]



-- REPORTS


react : RequestId -> Job -> Tx.Report -> Model -> ( Model, List Effect )
react id job txReport model =
    let
        drainedJob =
            { job | body = drained job.work txReport job.body }

        generic =
            hygiene txReport
    in
    case txReport.status of
        Tx.Paused ->
            case drainedJob.client of
                Waiting ->
                    case continue model id drainedJob txReport of
                        Just next ->
                            let
                                ( answered, afterAnswer, answering ) =
                                    applyAnswer next.answer drainedJob model
                            in
                            ( putJob id { answered | work = next.work, meter = next.meter } afterAnswer
                            , answering ++ [ Extend (Exchange id) next.program ]
                            )

                        Nothing ->
                            let
                                ( m, fx ) =
                                    report (Exchange id) [ Anomaly "a transaction paused where its plan has no pause" ] (putJob id drainedJob model)
                            in
                            ( m, fx ++ [ Abort id ] )

                Answered ->
                    -- Already aborted; the bridge is unwinding it.
                    ( putJob id drainedJob model, [ Abort id ] )

                Gone ->
                    ( putJob id drainedJob model, [ Abort id ] )

        Tx.Settled ->
            let
                ( reply, faults ) =
                    judge drainedJob txReport

                concluded =
                    conclude id drainedJob reply (generic ++ faults) model
            in
            case drainedJob.work of
                Delivering streamed ->
                    afterStream drainedJob.conn streamed (List.all isOk txReport.steps) concluded

                _ ->
                    concluded


{-| A transaction that settled: answer if anyone is waiting, and never
leave a waiting client unanswered.
-}
conclude : RequestId -> Job -> Maybe Response -> List Fault -> Model -> ( Model, List Effect )
conclude id job reply faults model =
    let
        ( reported, reports ) =
            report (Exchange id) faults model

        ( answered, afterAnswer, answering ) =
            applyAnswer (Maybe.map Inline reply |> Maybe.withDefault Unanswered) job reported

        ( settled, fallback ) =
            case answered.client of
                Waiting ->
                    let
                        ( m, fx ) =
                            report (Exchange id) [ Anomaly "a transaction settled without answering its client" ] afterAnswer

                        ( responded, sent ) =
                            respond job.conn (dispositionOf answered) (Response.problem Response.InternalServerError "the request ended without an answer" []) (count (\l -> { l | responded = l.responded + 1 }) m)
                    in
                    ( responded, fx ++ sent )

                Answered ->
                    ( afterAnswer, [] )

                Gone ->
                    -- Nobody to answer: a connection still carrying this
                    -- exchange (its peer only half-closed) is closed now.
                    abandon job.conn id afterAnswer
    in
    ( { settled | jobs = Dict.remove (key id) settled.jobs }, reports ++ answering ++ fallback )


abandon : ConnId -> RequestId -> Model -> ( Model, List Effect )
abandon conn id model =
    case Dict.get (connKey conn) model.conns of
        Just c ->
            if c.phase == Exchanging id then
                ( putConn conn { c | phase = Closing { lingering = False } } model, [ CloseConn conn ] )

            else
                ( model, [] )

        Nothing ->
            ( model, [] )


type Answer
    = Inline Response
    | ByStream
    | Unanswered


applyAnswer : Answer -> Job -> Model -> ( Job, Model, List Effect )
applyAnswer answer job model =
    case ( answer, job.client ) of
        ( Inline response, Waiting ) ->
            let
                ( responded, effects ) =
                    respond job.conn (dispositionOf job) response (count (\l -> { l | responded = l.responded + 1 }) model)
            in
            ( { job | client = Answered }, responded, effects )

        ( ByStream, Waiting ) ->
            -- The Stream step writes the head and the bytes itself.
            ( { job | client = Answered }, count (\l -> { l | responded = l.responded + 1 }) model, [] )

        ( Inline _, Answered ) ->
            ( job, model, [] )

        ( Inline _, Gone ) ->
            ( job, model, [] )

        ( ByStream, Answered ) ->
            ( job, model, [] )

        ( ByStream, Gone ) ->
            ( job, model, [] )

        ( Unanswered, _ ) ->
            ( job, model, [] )


{-| A body step that succeeded consumed the whole body.
-}
drained : Work -> Tx.Report -> BodyState -> BodyState
drained work txReport body =
    let
        succeeded n =
            case List.head (List.drop n txReport.steps) of
                Just (Ok _) ->
                    True

                _ ->
                    False
    in
    case work of
        Echoing ->
            if succeeded 0 then
                Drained

            else
                body

        ReceivingJson _ _ ->
            if succeeded 0 then
                Drained

            else
                body

        Committing spec ->
            case spec.fill of
                Commit.Streamed _ ->
                    if succeeded 1 then
                        Drained

                    else
                        body

                Commit.Buffered _ ->
                    body

        Fetching _ ->
            body

        OpeningBlob _ _ ->
            body

        Delivering _ ->
            body

        Finishing ->
            body


{-| What any settled report says about the transaction's own cleanup:
failed cleanups, and handles it forgot to close.
-}
hygiene : Tx.Report -> List Fault
hygiene txReport =
    List.concatMap
        (\cleanup ->
            case cleanup.outcome of
                Ok _ ->
                    []

                Err error ->
                    case ( cleanup.kind, cleanup.path ) of
                        ( "unlink", Just path ) ->
                            Verdict.orphanUnlessGone path error

                        _ ->
                            [ Unclassified ("cleanup " ++ cleanup.kind) error ]
        )
        txReport.cleanups
        ++ List.map (\h -> Anomaly ("transaction left handle " ++ h ++ " open; the bridge closed it")) txReport.leaked



-- PAUSES


type alias Continuation =
    { answer : Answer
    , work : Work
    , program : Plan.Program
    , meter : Maybe Meter
    }


continue : Model -> RequestId -> Job -> Tx.Report -> Maybe Continuation
continue model id job txReport =
    let
        config =
            model.config

        finishWith program answer =
            Just { answer = answer, work = Finishing, program = program, meter = Nothing }
    in
    case ( job.work, txReport.steps ) of
        ( ReceivingJson itemId suspended, [ Ok value ] ) ->
            let
                nothingHeld =
                    Plan.end (Plan.resume suspended)
            in
            case readJson value of
                Ok text ->
                    case Commit.parsePayload text of
                        Ok payload ->
                            let
                                spec =
                                    Commit.ingest model.layout id itemId payload
                            in
                            Just { answer = Unanswered, work = Committing spec, program = Commit.saga spec (Plan.resume suspended), meter = Nothing }

                        Err why ->
                            finishWith nothingHeld (Inline (Response.problem Response.BadRequest why []))

                Err problem ->
                    finishWith nothingHeld (Inline (bodyRejected problem))

        ( OpeningBlob _ suspended, [ Ok _, Ok stat ] ) ->
            let
                giveBack =
                    Plan.release (Plan.resume suspended)
            in
            case Decode.decodeValue statDecoder stat of
                Ok file ->
                    if not file.isFile then
                        finishWith giveBack (Inline (Response.problem Response.InternalServerError "the blob path is not a regular file" []))

                    else if file.size == 0 then
                        finishWith giveBack (Inline Response.emptyOctets)

                    else
                        let
                            closes =
                                not (persists model job.conn (dispositionOf job))
                        in
                        Just
                            { answer = ByStream
                            , work = Delivering { closes = closes }
                            , program =
                                Plan.resume suspended
                                    |> Plan.stream
                                        { conn = job.conn
                                        , length = file.size
                                        , head = Response.serializeStreamHead (Date.fromMillis model.clock.wall) closes file.size
                                        }
                                    |> Plan.release
                            , meter = Just { direction = Outbound, rate = config.deliveryRate, moved = 0, running = False, checkAt = 0, required = 0 }
                            }

                Err _ ->
                    Nothing

        _ ->
            Nothing


readJson : Decode.Value -> Result Request.BodyProblem Request.JsonText
readJson value =
    case Decode.decodeValue Request.bodyDecoder value of
        Ok body ->
            Request.jsonFrom body

        Err _ ->
            Err Request.BodyNotJson



-- SETTLEMENTS


judge : Job -> Tx.Report -> Verdict
judge job txReport =
    case job.work of
        Echoing ->
            case txReport.steps of
                [ Ok value ] ->
                    case readJson value of
                        Ok text ->
                            ( Just (Response.okJsonText text), [] )

                        Err problem ->
                            ( Just (bodyRejected problem), [] )

                [ Err error ] ->
                    bodyFailure error

                _ ->
                    Verdict.quiet

        ReceivingJson _ _ ->
            case txReport.steps of
                [ Err error ] ->
                    bodyFailure error

                _ ->
                    Verdict.quiet

        Committing spec ->
            Commit.judge spec txReport

        Fetching itemId ->
            case txReport.steps of
                [ Ok value ] ->
                    case Decode.decodeValue upstreamDecoder value of
                        Ok reply ->
                            ( Just (fromUpstream itemId reply), [] )

                        Err error ->
                            ( Just (Response.problem Response.InternalServerError "the I/O bridge broke its protocol" [])
                            , [ Anomaly ("an upstream reply that does not decode: " ++ Decode.errorToString error) ]
                            )

                [ Err error ] ->
                    fetchFailure error

                _ ->
                    Verdict.quiet

        OpeningBlob itemId _ ->
            case txReport.steps of
                [ Err error ] ->
                    openFailure itemId error

                [ Ok _, Err error ] ->
                    Verdict.diskFailure "reading blob metadata" error

                _ ->
                    Verdict.quiet

        Delivering _ ->
            case txReport.steps of
                [ Err error ] ->
                    case Io.classify error of
                        Io.Reset ->
                            Verdict.quiet

                        Io.Cancelled ->
                            Verdict.quiet

                        _ ->
                            ( Nothing, [ Unclassified "streaming a blob" error ] )

                _ ->
                    Verdict.quiet

        Finishing ->
            Verdict.quiet


bodyFailure : HostError -> Verdict
bodyFailure error =
    case Io.classify error of
        Io.TooLarge ->
            ( Just (Verdict.hostProblem Response.PayloadTooLarge "the body exceeded the size limit" error), [] )

        Io.Reset ->
            ( Just (Verdict.hostProblem Response.BadRequest "the request body was cut off" error), [] )

        Io.Malformed ->
            ( Just (Verdict.hostProblem Response.BadRequest "the chunked body is malformed" error), [] )

        Io.Cancelled ->
            Verdict.quiet

        Io.NoEntry ->
            Verdict.fatal "reading the request body" error

        Io.Exists ->
            Verdict.fatal "reading the request body" error

        Io.Denied ->
            Verdict.fatal "reading the request body" error

        Io.NoSpace ->
            Verdict.fatal "reading the request body" error

        Io.Exhausted ->
            Verdict.fatal "reading the request body" error

        Io.DeviceError ->
            Verdict.fatal "reading the request body" error

        Io.Refused ->
            Verdict.fatal "reading the request body" error

        Io.Unreachable ->
            Verdict.fatal "reading the request body" error

        Io.Unrecognized ->
            Verdict.fatal "reading the request body" error


fetchFailure : HostError -> Verdict
fetchFailure error =
    let
        badGateway detail =
            ( Just (Verdict.hostProblem Response.BadGateway detail error), [] )
    in
    case Io.classify error of
        Io.Refused ->
            badGateway "the upstream refused the connection"

        Io.Reset ->
            badGateway "the upstream reset the connection"

        Io.Unreachable ->
            badGateway "the upstream is unreachable"

        Io.Exhausted ->
            ( Just (Verdict.hostProblem Response.ServiceUnavailable "out of sockets calling the upstream" error), [] )

        Io.Cancelled ->
            Verdict.quiet

        Io.NoEntry ->
            Verdict.fatal "calling the upstream" error

        Io.Exists ->
            Verdict.fatal "calling the upstream" error

        Io.Denied ->
            Verdict.fatal "calling the upstream" error

        Io.NoSpace ->
            Verdict.fatal "calling the upstream" error

        Io.DeviceError ->
            Verdict.fatal "calling the upstream" error

        Io.TooLarge ->
            Verdict.fatal "calling the upstream" error

        Io.Malformed ->
            Verdict.fatal "calling the upstream" error

        Io.Unrecognized ->
            Verdict.fatal "calling the upstream" error


{-| ENOENT here is the one place a host error means "no such resource".
Anything unmodeled, such as ELOOP from a symlink loop, is a fatal 500: it
is not evidence that the blob is missing.
-}
openFailure : ItemId -> HostError -> Verdict
openFailure id error =
    case Io.classify error of
        Io.NoEntry ->
            ( Just
                (Response.problem Response.NotFound
                    ("no blob " ++ String.fromInt (Domain.itemIdToInt id))
                    [ ( "blobId", Encode.int (Domain.itemIdToInt id) ) ]
                )
            , []
            )

        Io.Denied ->
            Verdict.diskFailure "opening the blob" error

        Io.Exhausted ->
            Verdict.diskFailure "opening the blob" error

        Io.DeviceError ->
            Verdict.diskFailure "opening the blob" error

        Io.Cancelled ->
            Verdict.quiet

        Io.NoSpace ->
            Verdict.fatal "opening the blob" error

        Io.Exists ->
            Verdict.fatal "opening the blob" error

        Io.Refused ->
            Verdict.fatal "opening the blob" error

        Io.Reset ->
            Verdict.fatal "opening the blob" error

        Io.Unreachable ->
            Verdict.fatal "opening the blob" error

        Io.TooLarge ->
            Verdict.fatal "opening the blob" error

        Io.Malformed ->
            Verdict.fatal "opening the blob" error

        Io.Unrecognized ->
            Verdict.fatal "opening the blob" error



-- BEGINNING


type Outcome
    = Now Response
    | Start
        { work : Work
        , program : Plan.Program
        , deadlineMs : Int
        , meter : Maybe Meter
        }


declaredBody : Request -> BodyState
declaredBody request =
    case request.body of
        Request.NoBody ->
            NoBody

        Request.Length _ ->
            Unread

        Request.Chunked ->
            Unread


begin : RequestId -> ConnId -> Disposition -> Outcome -> Model -> ( Model, List Effect )
begin id conn shape outcome model =
    case outcome of
        Now response ->
            answerNow conn shape response model

        Start start ->
            ( putJob id
                { client = Waiting
                , conn = conn
                , work = start.work
                , body = shape.body
                , persistence = shape.persistence
                , meter = start.meter
                , deadline = model.clock.mono + start.deadlineMs
                }
                model
            , [ Begin (Exchange id) start.program ]
            )


answerNow : ConnId -> Disposition -> Response -> Model -> ( Model, List Effect )
answerNow conn shape response model =
    respond conn shape response (count (\l -> { l | responded = l.responded + 1 }) model)


route : Model -> RequestId -> ConnId -> Request -> Outcome
route model id conn request =
    case Route.parse request.method request.path of
        Err mismatch ->
            Now (rejectRoute request mismatch)

        Ok r ->
            dispatch model id conn request r


dispatch : Model -> RequestId -> ConnId -> Request -> Route -> Outcome
dispatch model id conn request r =
    let
        config =
            model.config

        inbound =
            Just { direction = Inbound, rate = config.transferRate, moved = 0, running = False, checkAt = 0, required = 0 }

        invite =
            if request.expectsContinue then
                Just Response.continue

            else
                Nothing

        within limit start =
            case request.body of
                Request.Length n ->
                    if n > limit then
                        -- Refused from the head alone: not one body byte read.
                        Now
                            (Response.problem Response.PayloadTooLarge
                                ("the declared body of " ++ String.fromInt n ++ " bytes exceeds " ++ String.fromInt limit)
                                [ ( "limitBytes", Encode.int limit ) ]
                            )

                    else
                        start

                Request.Chunked ->
                    start

                Request.NoBody ->
                    start

        framing =
            Io.reading config.chunked request.body

        reading =
            Plan.fresh |> Plan.readBody { conn = conn, framing = framing, maxBytes = config.jsonLimit, invite = invite }

        readBody work program =
            within config.jsonLimit <|
                Start
                    { work = work
                    , program = program
                    , deadlineMs = config.transferDeadlineMs
                    , meter = inbound
                    }
    in
    case r of
        Route.Health ->
            Now (Response.ok (Encode.object [ ( "status", Encode.string "ok" ) ]))

        Route.Item itemId ->
            Now (itemResponse itemId)

        Route.Stats ->
            Now (Response.ok (encodeStats model))

        Route.Echo ->
            readBody Echoing (Plan.end reading)

        Route.Ingest itemId ->
            let
                ( program, suspended ) =
                    Plan.suspend reading
            in
            readBody (ReceivingJson itemId suspended) program

        Route.Upload itemId ->
            let
                spec =
                    Commit.upload model.layout id itemId { conn = conn, framing = framing, maxBytes = config.uploadLimit, invite = invite }
            in
            within config.uploadLimit <|
                Start
                    { work = Committing spec
                    , program = Commit.saga spec Plan.fresh
                    , deadlineMs = config.transferDeadlineMs
                    , meter = inbound
                    }

        Route.Blob itemId ->
            let
                ( program, suspended ) =
                    Plan.fresh
                        |> Plan.openRead { handle = Io.handle (Request.idToString id) "blob", path = Layout.finalPath (Layout.blob model.layout itemId) }
                        |> Plan.stat
                        |> Plan.suspend
            in
            Start
                { work = OpeningBlob itemId suspended
                , program = program
                , deadlineMs = config.deadlineMs
                , meter = Nothing
                }

        Route.Delayed itemId ->
            Start
                { work = Fetching itemId
                , program =
                    Plan.fresh
                        |> Plan.httpGet
                            { url = config.upstream ++ "/items/" ++ String.fromInt (Domain.itemIdToInt itemId)
                            , maxBytes = maxUpstreamBytes
                            }
                        |> Plan.end
                , deadlineMs = config.deadlineMs
                , meter = Nothing
                }


rejectRoute : Request -> Route.Mismatch -> Response
rejectRoute request mismatch =
    case mismatch of
        Route.NoSuchPath ->
            Response.problem Response.NotFound
                ("no route matches " ++ request.path)
                [ ( "path", Encode.string request.path ) ]

        Route.MethodNotAllowed allowed ->
            Response.problem (Response.MethodNotAllowed allowed)
                (Request.methodToString request.method ++ " is not allowed on " ++ request.path)
                []

        Route.InvalidSegment error ->
            Response.problem Response.BadRequest
                ("path segment " ++ String.fromInt error.position ++ " must be " ++ error.expected)
                [ ( "segment", Encode.string error.segment )
                , ( "position", Encode.int error.position )
                , ( "expected", Encode.string error.expected )
                ]


itemResponse : ItemId -> Response
itemResponse id =
    case Domain.lookupItem id of
        Just item ->
            Response.ok (Domain.encodeItem item)

        Nothing ->
            Response.problem Response.NotFound
                ("no item with id " ++ String.fromInt (Domain.itemIdToInt id))
                [ ( "itemId", Encode.int (Domain.itemIdToInt id) ) ]


bodyRejected : Request.BodyProblem -> Response
bodyRejected problem =
    case problem of
        Request.BodyTooLarge limit ->
            Response.problem Response.PayloadTooLarge
                ("body exceeds " ++ String.fromInt limit ++ " bytes")
                [ ( "limitBytes", Encode.int limit ) ]

        Request.BodyNotUtf8 ->
            Response.problem Response.BadRequest "body is not valid UTF-8" []

        Request.BodyNotJson ->
            Response.problem Response.BadRequest "body is not a single valid JSON text" []


statDecoder : Decode.Decoder { size : Int, isFile : Bool }
statDecoder =
    Decode.map2 (\size isFile -> { size = size, isFile = isFile })
        (Decode.field "size" Decode.int)
        (Decode.field "isFile" Decode.bool)



-- UPSTREAM


type alias UpstreamReply =
    { status : Int
    , body : Request.Body
    }


upstreamDecoder : Decode.Decoder UpstreamReply
upstreamDecoder =
    Decode.map2 UpstreamReply
        (Decode.field "status" Decode.int)
        (Decode.field "body" Request.bodyDecoder)


{-| The upstream is another program on the other side of a network, so its
answer is parsed like any client input, and checked against what was asked.
-}
fromUpstream : ItemId -> UpstreamReply -> Response
fromUpstream expected reply =
    let
        badGateway detail =
            Response.problem Response.BadGateway detail [ ( "upstreamStatus", Encode.int reply.status ) ]
    in
    case ( reply.status, reply.body ) of
        ( 200, Request.Text text ) ->
            case Decode.decodeString Domain.itemDecoder text of
                Ok item ->
                    if item.id == expected then
                        Response.ok (Domain.encodeItem item)

                    else
                        Response.problem Response.BadGateway
                            "the upstream answered for a different item"
                            [ ( "expectedItemId", Encode.int (Domain.itemIdToInt expected) )
                            , ( "answeredItemId", Encode.int (Domain.itemIdToInt item.id) )
                            ]

                Err _ ->
                    badGateway "the upstream sent an item that does not decode"

        ( 200, Request.TooLarge limit ) ->
            badGateway ("the upstream body exceeded " ++ String.fromInt limit ++ " bytes")

        ( 200, Request.NotUtf8 ) ->
            badGateway "the upstream body is not UTF-8"

        ( 404, _ ) ->
            Response.problem Response.NotFound
                ("the upstream has no item " ++ String.fromInt (Domain.itemIdToInt expected))
                [ ( "itemId", Encode.int (Domain.itemIdToInt expected) ) ]

        ( status, _ ) ->
            badGateway ("the upstream answered " ++ String.fromInt status)



-- THE JANITOR


{-| A process killed mid-upload leaves its part behind. Nothing waits on a
part, so collecting corpses is housekeeping, not recovery: right after boot,
and then `sweepEveryMs` after each sweep ends, the janitor lists the parts
directory and removes every entry that

  - is a regular file whose name `Layout.partFromName` accepts; anything
    else in the directory is not ours and is never touched,
  - was last modified more than `staleAfterMs` before the listing was
    taken, as measured by the host clock that the bridge reads when it
    lists, which is the same clock that wrote the mtime (L-spec Corollary
    16.2: Elm's own clock may be hours stale on an idle server), and
  - is not the part of a job this process holds.

The last rule is what makes a sweep safe for this process, whatever the
clocks do. The second protects other processes sharing the directory: every
transaction that writes a part ends within `transferDeadlineMs` of its
start, so a part untouched for longer than `staleAfterMs` (which `init`
insists is longer) belongs to no transaction that can still commit it,
unless its process stalled for the whole difference. Even then nothing is
corrupted: that commit's link finds no part and fails with ENOENT.

-}
janitorReported : Tx.Report -> Model -> ( Model, List Effect )
janitorReported txReport model =
    let
        ( checked, hygieneFaults ) =
            report Janitor (hygiene txReport) model

        ( next, effects ) =
            case ( model.sweep, txReport.status ) of
                ( Preparing, Tx.Settled ) ->
                    prepared txReport checked

                ( Surveying suspended, Tx.Paused ) ->
                    surveyed suspended txReport checked

                ( Surveying _, Tx.Settled ) ->
                    case txReport.steps of
                        [ Err error ] ->
                            report Janitor [ Unclassified "listing the parts directory" error ] (idle checked)

                        _ ->
                            janitorBroke "a survey settled without its listing" checked

                ( Discarding doomed, Tx.Settled ) ->
                    discarded doomed txReport checked

                _ ->
                    janitorBroke "a janitor report that matches no janitor transaction" checked
    in
    ( next, hygieneFaults ++ effects )


{-| The bridge broke the protocol. A broken boot halts; a broken sweep is
given up. The extension ends the transaction if it is paused and is ignored
by the bridge if it is not.
-}
janitorBroke : String -> Model -> ( Model, List Effect )
janitorBroke detail model =
    let
        ( reported, faults ) =
            report Janitor [ Anomaly detail ] model
    in
    case model.sweep of
        Preparing ->
            ( { reported | phase = Stopped, sweep = Idle }, faults ++ [ Halt "the data directory could not be prepared" ] )

        _ ->
            ( idle reported, faults ++ [ Extend Janitor (Plan.end Plan.fresh) ] )


prepared : Tx.Report -> Model -> ( Model, List Effect )
prepared txReport model =
    case txReport.steps of
        [ Ok _, Ok _ ] ->
            case model.phase of
                Booting ->
                    let
                        ( program, suspended ) =
                            Plan.fresh |> Plan.survey model.layout |> Plan.suspend
                    in
                    ( { model | phase = Serving, sweep = Surveying suspended }, [ Listen, Begin Janitor program ] )

                Serving ->
                    janitorBroke "the layout was prepared twice" model

                Stopped ->
                    -- The server closed while booting: it never opens.
                    ( { model | sweep = Idle }, [] )

        steps ->
            case List.reverse steps of
                (Err error) :: _ ->
                    let
                        ( reported, faults ) =
                            report Janitor [ Unclassified "preparing the data directory" error ] { model | phase = Stopped, sweep = Idle }
                    in
                    ( reported, faults ++ [ Halt "the data directory could not be prepared" ] )

                _ ->
                    janitorBroke "the layout transaction settled without preparing the layout" model


surveyed : Plan.Suspended Plan.Fresh -> Tx.Report -> Model -> ( Model, List Effect )
surveyed suspended txReport model =
    let
        finish doomed =
            if List.isEmpty doomed then
                Plan.end (Plan.resume suspended)

            else
                Plan.resume suspended |> Plan.discard doomed |> Plan.end
    in
    case txReport.steps of
        [ Ok value ] ->
            case Decode.decodeValue listingDecoder value of
                Ok listing ->
                    let
                        ( doomed, unreadable ) =
                            judgeListing model listing

                        ( reported, faults ) =
                            report Janitor unreadable { model | sweep = Discarding doomed }
                    in
                    ( reported, faults ++ [ Extend Janitor (finish doomed) ] )

                Err error ->
                    let
                        ( reported, faults ) =
                            report Janitor [ Anomaly ("a listing that does not decode: " ++ Decode.errorToString error) ] { model | sweep = Discarding [] }
                    in
                    ( reported, faults ++ [ Extend Janitor (finish []) ] )

        _ ->
            let
                ( reported, faults ) =
                    report Janitor [ Anomaly "a survey paused without its listing" ] { model | sweep = Discarding [] }
            in
            ( reported, faults ++ [ Extend Janitor (finish []) ] )


discarded : List Part -> Tx.Report -> Model -> ( Model, List Effect )
discarded doomed txReport model =
    let
        rested =
            idle model
    in
    case ( doomed, txReport.steps ) of
        ( [], [] ) ->
            ( rested, [] )

        ( _, [ Ok value ] ) ->
            case Decode.decodeValue (Decode.field "outcomes" (Decode.list Io.outcomeDecoder)) value of
                Ok outcomes ->
                    if List.length outcomes /= List.length doomed then
                        report Janitor [ Anomaly "an unlink report that does not match what was doomed" ] rested

                    else
                        let
                            results =
                                List.map2 Tuple.pair doomed outcomes

                            removed =
                                List.length (List.filter (\( _, outcome ) -> isOk outcome) results)

                            faults =
                                List.concatMap
                                    (\( doomedPart, outcome ) ->
                                        case outcome of
                                            Ok _ ->
                                                []

                                            Err error ->
                                                Verdict.orphanUnlessGone (Layout.partPath doomedPart) error
                                    )
                                    results
                        in
                        report Janitor faults (count (\l -> { l | swept = l.swept + removed }) rested)

                Err error ->
                    report Janitor [ Anomaly ("an unlink report that does not decode: " ++ Decode.errorToString error) ] rested

        ( _, [ Err error ] ) ->
            report Janitor [ Unclassified "removing stale parts" error ] rested

        _ ->
            report Janitor [ Anomaly "a discard settled without the outcome of its unlinks" ] rested


isOk : Result e a -> Bool
isOk result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False


type alias Listing =
    { at : Int
    , entries : List Entry
    }


type alias Entry =
    { name : String
    , meta : Result HostError { isFile : Bool, mtimeMs : Float }
    }


listingDecoder : Decode.Decoder Listing
listingDecoder =
    let
        meta =
            Decode.map2 (\isFile mtimeMs -> { isFile = isFile, mtimeMs = mtimeMs })
                (Decode.field "isFile" Decode.bool)
                (Decode.field "mtimeMs" Decode.float)
    in
    Decode.map2 Listing
        (Decode.field "at" Decode.int)
        (Decode.field "entries"
            (Decode.list
                (Decode.map2 Entry
                    (Decode.field "name" Decode.string)
                    (Decode.field "stat"
                        (Decode.oneOf
                            [ Decode.field "ok" (Decode.map Ok meta)
                            , Decode.field "error" (Decode.map Err Io.hostErrorDecoder)
                            ]
                        )
                    )
                )
            )
        )


{-| The parts to remove, and what the listing could not tell us.
-}
judgeListing : Model -> Listing -> ( List Part, List Fault )
judgeListing model listing =
    let
        held =
            List.filterMap heldPart (Dict.values model.jobs)

        stale meta =
            meta.isFile && toFloat listing.at - meta.mtimeMs > toFloat model.config.staleAfterMs
    in
    List.foldr
        (\entry ( doomed, faults ) ->
            case ( Layout.partFromName model.layout entry.name, entry.meta ) of
                ( Nothing, _ ) ->
                    ( doomed, faults )

                ( Just candidate, Ok meta ) ->
                    if stale meta && not (List.member candidate held) then
                        ( candidate :: doomed, faults )

                    else
                        ( doomed, faults )

                ( Just candidate, Err error ) ->
                    case Io.classify error of
                        Io.NoEntry ->
                            -- Removed since the directory was read, by the
                            -- transaction that wrote it.
                            ( doomed, faults )

                        _ ->
                            ( doomed, Unclassified ("inspecting " ++ Layout.partPath candidate) error :: faults )
        )
        ( [], [] )
        listing.entries


heldPart : Job -> Maybe Part
heldPart job =
    case job.work of
        Committing spec ->
            Just spec.part

        Echoing ->
            Nothing

        ReceivingJson _ _ ->
            Nothing

        Fetching _ ->
            Nothing

        OpeningBlob _ _ ->
            Nothing

        Delivering _ ->
            Nothing

        Finishing ->
            Nothing



-- BOOKKEEPING


withJob : RequestId -> Model -> (Job -> ( Model, List Effect )) -> ( Model, List Effect )
withJob id model f =
    case Dict.get (key id) model.jobs of
        Just job ->
            f job

        Nothing ->
            -- Already settled; this event lost a race with the settlement.
            ( model, [] )


{-| A connection the server has forgotten has nothing left to decide: its
fd is closed.
-}
withConn : ConnId -> Model -> (Conn -> ( Model, List Effect )) -> ( Model, List Effect )
withConn conn model f =
    case Dict.get (connKey conn) model.conns of
        Just c ->
            f c

        Nothing ->
            ( model, [] )


key : RequestId -> String
key =
    Request.idToString


connKey : ConnId -> String
connKey =
    Request.connIdToString


putJob : RequestId -> Job -> Model -> Model
putJob id job model =
    { model | jobs = Dict.insert (key id) job model.jobs }


putConn : ConnId -> Conn -> Model -> Model
putConn conn c model =
    { model | conns = Dict.insert (connKey conn) c model.conns }


count : (Ledger -> Ledger) -> Model -> Model
count f model =
    { model | ledger = f model.ledger }


report : Owner -> List Fault -> Model -> ( Model, List Effect )
report owner faults model =
    ( count
        (\l ->
            { l
                | faults = l.faults + List.length faults
                , orphans = l.orphans + List.length (List.filter isOrphan faults)
            }
        )
        model
    , List.map (Report owner) faults
    )


isOrphan : Fault -> Bool
isOrphan fault =
    case fault of
        Orphaned _ _ ->
            True

        Unclassified _ _ ->
            False

        Anomaly _ ->
            False


encodeStats : Model -> Encode.Value
encodeStats model =
    let
        jobs =
            Dict.values model.jobs

        waiting =
            List.length (List.filter (\job -> job.client == Waiting) jobs)

        l =
            model.ledger
    in
    Encode.object
        [ ( "received", Encode.int l.received )
        , ( "responded", Encode.int l.responded )
        , ( "abandoned", Encode.int l.abandoned )
        , ( "timedOut", Encode.int l.timedOut )
        , ( "tooSlow", Encode.int l.tooSlow )
        , ( "aborted", Encode.int l.aborted )
        , ( "faults", Encode.int l.faults )
        , ( "orphans", Encode.int l.orphans )
        , ( "swept", Encode.int l.swept )
        , ( "connections", Encode.object [ ( "accepted", Encode.int l.accepted ), ( "open", Encode.int (Dict.size model.conns) ) ] )
        , ( "lingered"
          , Encode.object
                [ ( "hungUp", Encode.int l.lingered.hungUp )
                , ( "deadlinePassed", Encode.int l.lingered.deadlinePassed )
                , ( "budgetSpent", Encode.int l.lingered.budgetSpent )
                , ( "poolFull", Encode.int l.lingered.poolFull )
                , ( "bytesDiscarded", Encode.int l.lingered.bytesDiscarded )
                ]
          )
        , ( "inFlight", Encode.int waiting )
        , ( "draining", Encode.int (List.length jobs - waiting) )
        ]
