module Server exposing
    ( Answer(..)
    , Client(..)
    , Config
    , Direction(..)
    , Effect(..)
    , Job
    , Ledger
    , Meter
    , Model
    , Msg(..)
    , Rate
    , Work(..)
    , init
    , maxUpstreamBytes
    , step
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
    }


type alias Model =
    { config : Config
    , now : Int
    , jobs : Dict String Job
    , ledger : Ledger
    }


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
    }


{-| `Received` carries the host clock at arrival; `Tick` carries it on
every beat; `Progressed` carries the bridge's clock at the moment it took the
count, so a count and its index are written together (L-spec Corollary
16.2). All are milliseconds since the epoch.
-}
type Msg
    = Received RequestId Int Decode.Value
    | ClientGone RequestId
    | Reported RequestId Decode.Value
    | Progressed RequestId Int Bool Int
    | Tick Int


{-| Each becomes one entry in the single ordered instruction array `Main`
sends per update.
-}
type Effect
    = Respond RequestId Response Response.Connection
    | Begin RequestId Plan.Program
    | Extend RequestId Plan.Program
    | Abort RequestId
    | Report RequestId Fault


maxUpstreamBytes : Int
maxUpstreamBytes =
    65536


init : Config -> Model
init config =
    { config = config
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
        }
    }


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

        Reported id value ->
            case Dict.get (key id) model.jobs of
                Nothing ->
                    report id [ Anomaly "a report for a transaction nobody holds" ] model

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
                , [ Respond id response (connection job.body), Abort id ]
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
                    (connection job.body)
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
            cleanupFaults txReport ++ List.map (\h -> Anomaly ("transaction left handle " ++ h ++ " open; the bridge closed it")) txReport.leaked
    in
    case txReport.status of
        Tx.Paused ->
            case drainedJob.client of
                Waiting ->
                    case continue model.config id drainedJob txReport of
                        Just next ->
                            let
                                ( answered, afterAnswer, answering ) =
                                    applyAnswer id next.answer drainedJob model
                            in
                            ( putJob id { answered | work = next.work, meter = next.meter } afterAnswer
                            , answering ++ [ Extend id next.program ]
                            )

                        Nothing ->
                            let
                                ( m, fx ) =
                                    report id [ Anomaly "a transaction paused where its plan has no pause" ] (putJob id drainedJob model)
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
            report id faults model

        ( answered, afterAnswer, answering ) =
            applyAnswer id (Maybe.map Inline reply |> Maybe.withDefault Unanswered) job reported

        ( settled, fallback ) =
            case answered.client of
                Waiting ->
                    let
                        ( m, fx ) =
                            report id [ Anomaly "a transaction settled without answering its client" ] afterAnswer
                    in
                    ( count (\l -> { l | responded = l.responded + 1 }) m
                    , fx ++ [ Respond id (Response.problem Response.InternalServerError "the request ended without an answer" []) (connection answered.body) ]
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
            , [ Respond id response (connection job.body) ]
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


connection : BodyState -> Response.Connection
connection body =
    case body of
        Unread ->
            Response.CloseAfter

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


cleanupFaults : Tx.Report -> List Fault
cleanupFaults txReport =
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



-- PAUSES


type alias Continuation =
    { answer : Answer
    , work : Work
    , program : Plan.Program
    , meter : Maybe Meter
    }


continue : Config -> RequestId -> Job -> Tx.Report -> Maybe Continuation
continue config id job txReport =
    let
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
                                    Commit.ingest id config.dataDir itemId payload
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
            , [ Begin id start.program ]
            )


answerNow : RequestId -> BodyState -> Response -> Model -> ( Model, List Effect )
answerNow id body response model =
    ( count (\l -> { l | responded = l.responded + 1 }) model
    , [ Respond id response (connection body) ]
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
                    Commit.upload id config.dataDir itemId { maxBytes = config.uploadLimit, sendContinue = request.expectsContinue }
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
                        |> Plan.openRead { handle = Io.handle (Request.idToString id) "blob", path = blobPath config.dataDir itemId }
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


blobPath : String -> ItemId -> String
blobPath dir id =
    dir ++ "/blob_" ++ String.fromInt (Domain.itemIdToInt id) ++ ".bin"


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


report : RequestId -> List Fault -> Model -> ( Model, List Effect )
report id faults model =
    ( count
        (\l ->
            { l
                | faults = l.faults + List.length faults
                , orphans = l.orphans + List.length (List.filter isOrphan faults)
            }
        )
        model
    , List.map (Report id) faults
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
        , ( "inFlight", Encode.int waiting )
        , ( "draining", Encode.int (List.length jobs - waiting) )
        ]
