module Server exposing
    ( Answer(..)
    , Client(..)
    , Config
    , Direction(..)
    , Effect(..)
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
    , init
    , maxUpstreamBytes
    , ownerFromKey
    , ownerKey
    , step
    , sweepEvery
    )

{-| The router as a pure state machine.

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

A job exists exactly while its transaction is unsettled. No hung requests,
by construction: every job carries a deadline from the moment it begins, and
`conclude` answers 500 if a transaction settles while its client is still
waiting.

Time is data, not timers. A job stores its deadline and its meter's next
check as absolute instants; `Tick` (a subscription that exists only while
jobs do) compares them with the clock. An Elm `Process.sleep` cannot be
cancelled, so arming one per request would leave a timer alive long after
the request is gone.

The server opens only once its layout exists: `init` begins a transaction
that creates the parts and blob directories, and `Listen` goes out only
when it settles cleanly. Until then the bridge is not listening, so no
request can reach a lease whose directory is missing. The same janitor then
collects the parts that crashed processes left behind; see THE JANITOR.

-}

import Commit
import Dict exposing (Dict)
import Domain exposing (ItemId)
import Http.Request as Request exposing (Request, RequestId)
import Http.Response as Response exposing (Response)
import Http.Route as Route exposing (Route)
import Io exposing (Handle, HostError)
import Json.Decode as Decode
import Json.Encode as Encode
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
    , linger : Response.Linger
    }


type alias Model =
    { config : Config
    , layout : Layout
    , phase : Phase
    , sweep : Sweep
    , now : Int
    , jobs : Dict String Job
    , ledger : Ledger
    }


{-| `Stopped` is final: the layout could not be prepared, or the bridge's
server closed. Jobs still in flight drain; nothing new is swept.
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


{-| Who a transaction belongs to: a client's exchange, or the server.
-}
type Owner
    = Exchange RequestId
    | Janitor


type alias Job =
    { client : Client
    , work : Work
    , body : BodyState
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


{-| What the transaction's next report means.
-}
type Work
    = Echoing
    | ReceivingJson ItemId (Plan.Suspended Plan.Fresh)
    | Committing Commit.Spec
    | Fetching ItemId
    | OpeningBlob ItemId (Plan.Suspended Plan.Reading)
    | Delivering
    | Closing


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
    , lingered : Lingered
    }


{-| How connections refused with an unread body ended (see
`Response.Linger`): the client hung up before any bound was crossed, or the
deadline or the byte budget cut it, or the pool was full and the close was
immediate. And how many bytes the bridge read and dropped for them.
-}
type alias Lingered =
    { hungUp : Int
    , deadlinePassed : Int
    , budgetSpent : Int
    , poolFull : Int
    , bytesDiscarded : Int
    }


{-| `Received` carries the host clock at arrival; `Tick` carries it on
every beat; `Progressed` carries the bridge's clock at the moment it took the
count, so a count and its index are written together (L-spec Corollary
16.2). All are milliseconds since the epoch. `SweepDue` is the janitor's
alarm and `Closed` is the bridge's server shutting down. `Released` comes
once per refused connection, after its fd is closed, and is only counted:
that connection is no longer anything the server decides about.
-}
type Msg
    = Received RequestId Int Decode.Value
    | ClientGone RequestId
    | Reported Owner Decode.Value
    | Progressed RequestId Int Bool Int
    | Tick Int
    | SweepDue
    | Closed
    | Released RequestId Decode.Value


{-| Each becomes one entry in the single ordered instruction array `Main`
sends per update.
-}
type Effect
    = Respond RequestId Response Response.Connection
    | Begin Owner Plan.Program
    | Extend Owner Plan.Program
    | Abort RequestId
    | Report Owner Fault
    | Listen
    | Halt String


{-| A transaction's name on the wire. A request's is its request id, which
the bridge mints with `randomUUID`; "janitor" is not a UUID, so the two
names can never meet, and `ownerFromKey` inverts `ownerKey`.
-}
ownerKey : Owner -> String
ownerKey owner =
    case owner of
        Exchange id ->
            Request.idToString id

        Janitor ->
            "janitor"


ownerFromKey : String -> Owner
ownerFromKey name =
    if name == "janitor" then
        Janitor

    else
        Exchange (Request.idFromString name)


{-| How often the janitor's alarm should ring, if it should: only while
serving and between sweeps. Like `Tick`, it is a subscription that exists
only while it has work, so a closed server leaves no timer behind.
-}
sweepEvery : Model -> Maybe Int
sweepEvery model =
    case ( model.phase, model.sweep ) of
        ( Serving, Idle ) ->
            Just model.config.sweepEveryMs

        _ ->
            Nothing


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
            , now = 0
            , jobs = Dict.empty
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

    else if config.sweepEveryMs <= 0 then
        Just "sweepEveryMs must be positive"

    else if config.linger.deadlineMs < 0 || config.linger.maxBytes < 0 || config.linger.maxSockets < 0 then
        Just "the linger bounds must not be negative"

    else
        Nothing


step : Msg -> Model -> ( Model, List Effect )
step msg model =
    case msg of
        Received id at wire ->
            let
                counted =
                    count (\l -> { l | received = l.received + 1 }) { model | now = at }
            in
            case Request.fromWire wire of
                Err rejection ->
                    -- Nothing is known about this request's body; close.
                    answerNow id Unread (rejectWire rejection) counted

                Ok request ->
                    -- Routes see the model before this request is counted, so
                    -- /stats reports a ledger that balances.
                    begin id (declaredBody request) (route model id request) counted

        ClientGone id ->
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

        SweepDue ->
            case ( model.phase, model.sweep ) of
                ( Serving, Idle ) ->
                    let
                        ( program, suspended ) =
                            Plan.fresh |> Plan.survey model.layout |> Plan.suspend
                    in
                    ( { model | sweep = Surveying suspended }, [ Begin Janitor program ] )

                _ ->
                    -- An alarm already queued when the last sweep began or
                    -- the server stopped.
                    ( model, [] )

        Closed ->
            ( { model | phase = Stopped }, [] )

        Released id value ->
            case Decode.decodeValue releaseDecoder value of
                Ok ( how, bytes ) ->
                    ( count (\l -> { l | lingered = tally how bytes l.lingered }) model, [] )

                Err error ->
                    report (Exchange id) [ Anomaly ("a connection release that does not decode: " ++ Decode.errorToString error) ] model

        Progressed id bytes running at ->
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
                                                    , checkAt = at + meter.rate.perMs
                                                    , required = bytes + meter.rate.bytes
                                                }
                                    }
                                    model
                                , []
                                )

                            else
                                ( putJob id { job | meter = Just { meter | running = running, moved = bytes } } model, [] )

        Tick now ->
            Dict.foldl
                (\owner _ ( m, effects ) ->
                    let
                        ( next, more ) =
                            expire (Request.idFromString owner) m
                    in
                    ( next, effects ++ more )
                )
                ( { model | now = now }, [] )
                model.jobs

        Reported Janitor value ->
            case Decode.decodeValue Tx.reportDecoder value of
                Ok txReport ->
                    janitorReported txReport model

                Err error ->
                    janitorBroke ("a report that does not decode: " ++ Decode.errorToString error) model

        Reported (Exchange id) value ->
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


{-| The clock reached this job: its deadline, then its meter.
-}
expire : RequestId -> Model -> ( Model, List Effect )
expire id model =
    withJob id model <|
        \job ->
            if job.client == Waiting && model.now >= job.deadline then
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
                in
                ( model
                    |> putJob id { job | client = Answered }
                    |> count (\l -> { l | responded = l.responded + 1, timedOut = l.timedOut + 1, aborted = l.aborted + 1 })
                , [ Respond id response (connection model.config.linger job.body), Abort id ]
                )

            else
                case job.meter of
                    Just meter ->
                        -- A count may trail the bytes by up to progressEveryMs,
                        -- so a window is judged only once every byte that arrived
                        -- in it must have been reported. Judging earlier reads a
                        -- stale count as the present (L-spec Theorem 14).
                        if not meter.running || model.now < meter.checkAt + model.config.progressEveryMs then
                            ( model, [] )

                        else if meter.moved >= meter.required then
                            ( putJob id
                                { job
                                    | meter =
                                        Just
                                            { meter
                                                | checkAt = model.now + meter.rate.perMs
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
            -- The abort is what ends the body pipe: once a response has
            -- finished, Node stops destroying the request stream when the
            -- socket dies, so a pipe still reading it would wait forever.
            -- It does not end the connection (Node's pipeline detaches a
            -- server request's socket before destroying it); the 408's
            -- `connection: close` does that, after the client has it.
            ( slow
                |> putJob id { job | client = Answered, meter = Just { meter | running = False } }
                |> count (\l -> { l | responded = l.responded + 1 })
            , [ Respond id
                    (Response.problem Response.RequestTimeout
                        ("the body arrived slower than " ++ String.fromInt meter.rate.bytes ++ " bytes per " ++ String.fromInt meter.rate.perMs ++ " ms")
                        [ ( "bytesReceived", Encode.int meter.moved ) ]
                    )
                    (connection model.config.linger job.body)
              , Abort id
              ]
            )

        ( Inbound, Answered ) ->
            ( model, [] )

        ( Inbound, Gone ) ->
            ( model, [] )

        ( Outbound, _ ) ->
            -- The status line is long gone; all that is left is the fd.
            ( putJob id { job | meter = Just { meter | running = False } } slow, [ Abort id ] )



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
                                    applyAnswer id next.answer drainedJob model
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
            in
            conclude id drainedJob reply (generic ++ faults) model


{-| A transaction that settled: answer if anyone is waiting, and never
leave a waiting client unanswered.
-}
conclude : RequestId -> Job -> Maybe Response -> List Fault -> Model -> ( Model, List Effect )
conclude id job reply faults model =
    let
        ( reported, reports ) =
            report (Exchange id) faults model

        ( answered, afterAnswer, answering ) =
            applyAnswer id (Maybe.map Inline reply |> Maybe.withDefault Unanswered) job reported

        ( settled, fallback ) =
            case answered.client of
                Waiting ->
                    let
                        ( m, fx ) =
                            report (Exchange id) [ Anomaly "a transaction settled without answering its client" ] afterAnswer
                    in
                    ( count (\l -> { l | responded = l.responded + 1 }) m
                    , fx ++ [ Respond id (Response.problem Response.InternalServerError "the request ended without an answer" []) (connection model.config.linger answered.body) ]
                    )

                Answered ->
                    ( afterAnswer, [] )

                Gone ->
                    ( afterAnswer, [] )
    in
    ( { settled | jobs = Dict.remove (key id) settled.jobs }, reports ++ answering ++ fallback )


type Answer
    = Inline Response
    | ByStream
    | Unanswered


applyAnswer : RequestId -> Answer -> Job -> Model -> ( Job, Model, List Effect )
applyAnswer id answer job model =
    case ( answer, job.client ) of
        ( Inline response, Waiting ) ->
            ( { job | client = Answered }
            , count (\l -> { l | responded = l.responded + 1 }) model
            , [ Respond id response (connection model.config.linger job.body) ]
            )

        ( ByStream, Waiting ) ->
            -- The Stream step writes the status line itself.
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


connection : Response.Linger -> BodyState -> Response.Connection
connection linger body =
    case body of
        Unread ->
            Response.CloseAfter linger

        NoBody ->
            Response.KeepAlive

        Drained ->
            Response.KeepAlive


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

        Delivering ->
            body

        Closing ->
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
            Just { answer = answer, work = Closing, program = program, meter = Nothing }
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
                            head =
                                Response.streamHead file.size
                        in
                        Just
                            { answer = ByStream
                            , work = Delivering
                            , program =
                                Plan.resume suspended
                                    |> Plan.stream { length = file.size, status = head.status, headers = head.headers }
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

        Delivering ->
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

        Closing ->
            Verdict.quiet


bodyFailure : HostError -> Verdict
bodyFailure error =
    case Io.classify error of
        Io.TooLarge ->
            ( Just (Verdict.hostProblem Response.PayloadTooLarge "the body exceeded the size limit" error), [] )

        Io.Reset ->
            ( Just (Verdict.hostProblem Response.BadRequest "the request body was cut off" error), [] )

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

        Request.Unsized ->
            Unread


begin : RequestId -> BodyState -> Outcome -> Model -> ( Model, List Effect )
begin id body outcome model =
    case outcome of
        Now response ->
            answerNow id body response model

        Start start ->
            ( putJob id { client = Waiting, work = start.work, body = body, meter = start.meter, deadline = model.now + start.deadlineMs } model
            , [ Begin (Exchange id) start.program ]
            )


answerNow : RequestId -> BodyState -> Response -> Model -> ( Model, List Effect )
answerNow id body response model =
    ( count (\l -> { l | responded = l.responded + 1 }) model
    , [ Respond id response (connection model.config.linger body) ]
    )


rejectWire : Request.Rejection -> Response
rejectWire rejection =
    case rejection of
        Request.Malformed error ->
            Response.problem Response.InternalServerError
                "the bridge sent a request the router could not decode"
                [ ( "decoderError", Encode.string (Decode.errorToString error) ) ]

        Request.UnsupportedMethod method ->
            Response.problem Response.NotImplemented
                ("method " ++ method ++ " is not implemented")
                [ ( "method", Encode.string method ) ]


route : Model -> RequestId -> Request -> Outcome
route model id request =
    case Route.parse request.method request.path of
        Err mismatch ->
            Now (rejectRoute request mismatch)

        Ok r ->
            dispatch model id request r


dispatch : Model -> RequestId -> Request -> Route -> Outcome
dispatch model id request r =
    let
        config =
            model.config

        inbound =
            Just { direction = Inbound, rate = config.transferRate, moved = 0, running = False, checkAt = 0, required = 0 }

        within limit start =
            case request.body of
                Request.Length n ->
                    if n > limit then
                        -- Refused from the headers alone: not one body byte read.
                        Now
                            (Response.problem Response.PayloadTooLarge
                                ("the declared body of " ++ String.fromInt n ++ " bytes exceeds " ++ String.fromInt limit)
                                [ ( "limitBytes", Encode.int limit ) ]
                            )

                    else
                        start

                Request.Unsized ->
                    start

                Request.NoBody ->
                    start

        reading =
            Plan.fresh |> Plan.readBody { maxBytes = config.jsonLimit, sendContinue = request.expectsContinue }

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
                    Commit.upload model.layout id itemId { maxBytes = config.uploadLimit, sendContinue = request.expectsContinue }
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
                            report Janitor [ Unclassified "listing the parts directory" error ] { checked | sweep = Idle }

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
            ( { reported | sweep = Idle }, faults ++ [ Extend Janitor (Plan.end Plan.fresh) ] )


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
        idle =
            { model | sweep = Idle }
    in
    case ( doomed, txReport.steps ) of
        ( [], [] ) ->
            ( idle, [] )

        ( _, [ Ok value ] ) ->
            case Decode.decodeValue (Decode.field "outcomes" (Decode.list Io.outcomeDecoder)) value of
                Ok outcomes ->
                    if List.length outcomes /= List.length doomed then
                        report Janitor [ Anomaly "an unlink report that does not match what was doomed" ] idle

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
                        report Janitor faults (count (\l -> { l | swept = l.swept + removed }) idle)

                Err error ->
                    report Janitor [ Anomaly ("an unlink report that does not decode: " ++ Decode.errorToString error) ] idle

        ( _, [ Err error ] ) ->
            report Janitor [ Unclassified "removing stale parts" error ] idle

        _ ->
            report Janitor [ Anomaly "a discard settled without the outcome of its unlinks" ] idle


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

        Delivering ->
            Nothing

        Closing ->
            Nothing



-- LINGERING CONNECTIONS


type Release
    = HungUp
    | DeadlinePassed
    | BudgetSpent
    | PoolFull


releaseDecoder : Decode.Decoder ( Release, Int )
releaseDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "how" Decode.string
            |> Decode.andThen
                (\how ->
                    case how of
                        "hungUp" ->
                            Decode.succeed HungUp

                        "deadline" ->
                            Decode.succeed DeadlinePassed

                        "budget" ->
                            Decode.succeed BudgetSpent

                        "poolFull" ->
                            Decode.succeed PoolFull

                        _ ->
                            Decode.fail ("no such release: " ++ how)
                )
        )
        (Decode.field "discarded" Decode.int)


tally : Release -> Int -> Lingered -> Lingered
tally how bytes lingered =
    let
        counted =
            { lingered | bytesDiscarded = lingered.bytesDiscarded + bytes }
    in
    case how of
        HungUp ->
            { counted | hungUp = counted.hungUp + 1 }

        DeadlinePassed ->
            { counted | deadlinePassed = counted.deadlinePassed + 1 }

        BudgetSpent ->
            { counted | budgetSpent = counted.budgetSpent + 1 }

        PoolFull ->
            { counted | poolFull = counted.poolFull + 1 }



-- BOOKKEEPING


withJob : RequestId -> Model -> (Job -> ( Model, List Effect )) -> ( Model, List Effect )
withJob id model f =
    case Dict.get (key id) model.jobs of
        Just job ->
            f job

        Nothing ->
            -- Already settled; this event lost a race with the settlement.
            ( model, [] )


key : RequestId -> String
key =
    Request.idToString


putJob : RequestId -> Job -> Model -> Model
putJob id job model =
    { model | jobs = Dict.insert (key id) job model.jobs }


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
