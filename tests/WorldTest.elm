module WorldTest exposing (suite)

{-| Model-based test: `Server.step` against a simulated host.

Each request's transaction runs on the reference interpreter (`Tx.Machine`),
standing in for the bridge. Under it, a fake filesystem with Linux semantics
(a failed open creates nothing, a failed write leaves a partial file, close
always frees the fd, link is create-only) and fake sockets that can die
mid-body. Every operation can fail with an injected error; completions,
deadlines, departures, progress reports and rate checks arrive in random
interleavings. Afterwards everything is drained and the world is checked
for leaks, orphans, lies and hung requests.

-}

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Http.Request as Request
import Http.Response as Response
import Io exposing (HostError, Op)
import Json.Decode as Decode
import Json.Encode as Encode
import Plan
import Server exposing (Effect(..), Msg(..))
import Set exposing (Set)
import Test exposing (Test, describe, fuzzWith)
import Test.Distribution
import Tx
import Verdict


config : Server.Config
config =
    { dataDir = "/d"
    , upstream = "http://up"
    , deadlineMs = 2000
    , transferDeadlineMs = 3000
    , transferRate = { bytes = 4096, perMs = 1000 }
    , deliveryRate = { bytes = 4096, perMs = 1000 }
    , jsonLimit = 64
    , uploadLimit = 64
    , progressEveryMs = 100
    }


type alias Info =
    { body : String
    , declared : Bool
    , final : Maybe String
    }


type alias World =
    { model : Server.Model
    , machines : Dict String Tx.Machine
    , files : Dict String String
    , fds : Dict String String
    , responses : Dict String (List Int)
    , faults : List ( String, Verdict.Fault )
    , issued : Int
    , requests : Dict String Info
    , drained : Set String
    , left : Set String
    , over : Set String
    , clock : Int
    , moved : Dict String Int
    , injectedInto : Set String
    , outbox : List Effect
    , violations : List String
    }


emptyWorld : World
emptyWorld =
    { model = Server.init config
    , machines = Dict.empty
    , files = Dict.fromList [ ( "/d/blob_1.bin", "BLOBDATA" ) ]
    , fds = Dict.empty
    , responses = Dict.empty
    , faults = []
    , issued = 0
    , requests = Dict.empty
    , drained = Set.empty
    , left = Set.empty
    , over = Set.empty
    , clock = 0
    , moved = Dict.empty
    , injectedInto = Set.empty
    , outbox = []
    , violations = []
    }



-- CHOICES


type Choice
    = Arrive Int
    | Complete Int (Maybe Int)
    | Leave Int
    | Push Int Int
    | Wait Int
    | Deliver Int


injectable : List String
injectable =
    [ "ENOSPC", "EIO", "EACCES", "EWEIRD", "EMFILE", "ENOENT" ]


choice : Fuzzer Choice
choice =
    Fuzz.frequency
        [ ( 3, Fuzz.map Arrive (Fuzz.intRange 0 8) )
        , ( 8
          , Fuzz.map2 Complete
                (Fuzz.intRange 0 20)
                (Fuzz.frequency
                    [ ( 5, Fuzz.constant Nothing )
                    , ( 1, Fuzz.map Just (Fuzz.intRange 0 (List.length injectable - 1)) )
                    ]
                )
          )
        , ( 1, Fuzz.map Leave (Fuzz.intRange 0 40) )
        , ( 2, Fuzz.map2 Push (Fuzz.intRange 0 20) (Fuzz.intRange 0 6000) )
        , ( 2, Fuzz.map Wait (Fuzz.oneOfValues [ 50, 400, 1100, 2100, 3100 ]) )
        , ( 4, Fuzz.map Deliver (Fuzz.intRange 1 4) )
        ]


requestId : Int -> String
requestId n =
    "r" ++ String.fromInt n


arrive : Int -> World -> World
arrive template world =
    let
        id =
            requestId world.issued

        n =
            String.fromInt world.issued

        ( ( method, target ), info ) =
            case template of
                0 ->
                    ( ( "POST", "/ingest/1" ), { body = "{\"r\":" ++ n ++ "}", declared = True, final = Just "/d/file_1.final" } )

                1 ->
                    ( ( "POST", "/ingest/2" ), { body = "{\"r\":" ++ n ++ "}", declared = True, final = Just "/d/file_2.final" } )

                2 ->
                    ( ( "PUT", "/blobs/3" ), { body = "BLOB-" ++ n, declared = True, final = Just "/d/blob_3.bin" } )

                3 ->
                    ( ( "PUT", "/blobs/3" ), { body = "BLOB-" ++ n ++ String.repeat 70 "x", declared = False, final = Just "/d/blob_3.bin" } )

                4 ->
                    ( ( "GET", "/blobs/1" ), { body = "", declared = False, final = Nothing } )

                5 ->
                    ( ( "GET", "/blobs/2" ), { body = "", declared = False, final = Nothing } )

                6 ->
                    ( ( "GET", "/delayed/42" ), { body = "", declared = False, final = Nothing } )

                7 ->
                    ( ( "POST", "/echo" ), { body = "{\"echo\":" ++ n ++ "}", declared = True, final = Nothing } )

                _ ->
                    ( ( "GET", "/health" ), { body = "", declared = False, final = Nothing } )

        headers =
            if info.declared then
                [ "Content-Length", String.fromInt (String.length info.body) ]

            else if method == "PUT" then
                -- chunked: no declared length, so only the stream can find it too big
                [ "Transfer-Encoding", "chunked" ]

            else
                []

        wire =
            Encode.object
                [ ( "method", Encode.string method )
                , ( "target", Encode.string target )
                , ( "headers", Encode.list Encode.string headers )
                ]
    in
    feed (Received (Request.idFromString id) world.clock wire)
        { world | issued = world.issued + 1, requests = Dict.insert id info world.requests }


inFlight : World -> List ( String, Op )
inFlight world =
    Dict.toList world.machines
        |> List.filterMap (\( id, m ) -> Tx.inFlight m |> Maybe.map (Tuple.pair id))


apply : Choice -> World -> World
apply c world =
    let
        pickOf list i =
            if List.isEmpty list then
                Nothing

            else
                List.head (List.drop (modBy (List.length list) i) list)

        anyRequest i =
            if world.issued == 0 then
                Nothing

            else
                Just (requestId (modBy world.issued i))
    in
    case c of
        Arrive template ->
            arrive template world

        Complete i failure ->
            case pickOf (inFlight world) i of
                Just ( id, _ ) ->
                    complete (Maybe.andThen (\k -> List.head (List.drop k injectable)) failure) id world

                Nothing ->
                    world

        Leave i ->
            case anyRequest i of
                Just id ->
                    let
                        departed =
                            { world | left = Set.insert id world.left }
                    in
                    -- The bridge reports a disconnect only while it still
                    -- holds the exchange: before any answer was written.
                    if Set.member id world.over then
                        departed

                    else
                        feed (ClientGone (Request.idFromString id)) departed

                Nothing ->
                    world

        Push i k ->
            case pickOf (List.filter (\( _, op ) -> streaming op) (inFlight world)) i of
                Just ( id, _ ) ->
                    let
                        total =
                            Maybe.withDefault 0 (Dict.get id world.moved) + k
                    in
                    feed (Progressed (Request.idFromString id) total True world.clock) { world | moved = Dict.insert id total world.moved }

                Nothing ->
                    world

        Wait ms ->
            feed (Tick (world.clock + ms)) { world | clock = world.clock + ms }

        Deliver k ->
            deliver k world


{-| Elm's instructions reach the bridge through a port mailbox, in order but
late: the bridge can report, and the client can leave, in between.
-}
deliver : Int -> World -> World
deliver k world =
    case world.outbox of
        [] ->
            world

        effect :: rest ->
            if k <= 0 then
                world

            else
                deliver (k - 1) (carryOut effect { world | outbox = rest })


streaming : Op -> Bool
streaming op =
    case op of
        Io.ReadBody _ ->
            True

        Io.PipeBody _ ->
            True

        Io.Stream _ ->
            True

        _ ->
            False


{-| The host finishes the request's in-flight operation.
-}
complete : Maybe String -> String -> World -> World
complete failure id world =
    case Dict.get id world.machines of
        Just machine ->
            case Tx.inFlight machine of
                Just op ->
                    let
                        ( executed, outcome ) =
                            execute (Tx.signalled machine) failure id op world

                        marked =
                            case failure of
                                Just _ ->
                                    { executed | injectedInto = Set.insert id executed.injectedInto }

                                Nothing ->
                                    executed

                        stopped =
                            if streaming op then
                                feed (Progressed (Request.idFromString id) (Maybe.withDefault 0 (Dict.get id marked.moved)) False marked.clock) marked

                            else
                                marked
                    in
                    case Dict.get id stopped.machines of
                        Just current ->
                            transition id (Tx.land outcome current) stopped

                        Nothing ->
                            stopped

                Nothing ->
                    world

        Nothing ->
            world



-- THE FAKE HOST


ok : List ( String, Encode.Value ) -> Result HostError Decode.Value
ok fields =
    Ok (Encode.object fields)


err : String -> String -> Result HostError Decode.Value
err code syscall =
    Err
        (Decode.decodeValue Io.hostErrorDecoder
            (Encode.object
                [ ( "name", Encode.string "Error" )
                , ( "code", Encode.string code )
                , ( "syscall", Encode.string syscall )
                , ( "rawMessage", Encode.string (code ++ ": injected") )
                , ( "cause", Encode.null )
                ]
            )
            |> Result.withDefault Io.revokedError
        )


execute : Bool -> Maybe String -> String -> Op -> World -> ( World, Result HostError Decode.Value )
execute signalled injected id op world =
    let
        failure =
            Maybe.map
                (\code ->
                    case ( op, code ) of
                        ( Io.Open _, _ ) ->
                            code

                        -- ENOENT is only physically possible on open.
                        ( _, "ENOENT" ) ->
                            "EIO"

                        _ ->
                            code
                )
                injected

        info =
            Dict.get id world.requests |> Maybe.withDefault { body = "", declared = False, final = Nothing }

        socketDead =
            Set.member id world.left

        withFd handle run =
            case Dict.get (Io.handleToString handle) world.fds of
                Just path ->
                    run path

                Nothing ->
                    ( violate ("operation on an fd that is not open: " ++ Io.handleToString handle) world, err "EBADF" "fd" )

        failOr syscall w success =
            case failure of
                Just code ->
                    ( w, err code syscall )

                Nothing ->
                    success

        partial path w =
            { w | files = Dict.insert path (String.left (String.length info.body // 2) info.body) w.files }

        consumed w =
            { w | drained = Set.insert id w.drained }
    in
    if signalled && Io.interruptible op then
        case op of
            Io.PipeBody pipe ->
                withFd pipe.handle (\path -> ( partial path world, Err Io.revokedError ))

            _ ->
                ( world, Err Io.revokedError )

    else
        case op of
            Io.Open open ->
                failOr "open" world <|
                    case ( open.mode, Dict.member open.path world.files ) of
                        ( Io.CreateExclusive, True ) ->
                            ( world, err "EEXIST" "open" )

                        ( Io.CreateExclusive, False ) ->
                            ( { world | files = Dict.insert open.path "" world.files, fds = Dict.insert (Io.handleToString open.handle) open.path world.fds }, ok [] )

                        ( Io.ReadOnly, True ) ->
                            ( { world | fds = Dict.insert (Io.handleToString open.handle) open.path world.fds }, ok [] )

                        ( Io.ReadOnly, False ) ->
                            ( world, err "ENOENT" "open" )

            Io.Write handle text ->
                withFd handle <|
                    \path ->
                        case failure of
                            Just code ->
                                ( { world | files = Dict.insert path (String.left (String.length text // 2) text) world.files }, err code "write" )

                            Nothing ->
                                ( { world | files = Dict.insert path text world.files }
                                , ok [ ( "bytes", Encode.int (String.length text) ), ( "sha256", Encode.string ("sha:" ++ text) ) ]
                                )

            Io.PipeBody pipe ->
                withFd pipe.handle <|
                    \path ->
                        if socketDead then
                            ( partial path world, err "ECONNRESET" "read" )

                        else
                            case failure of
                                Just code ->
                                    ( partial path world, err code "write" )

                                Nothing ->
                                    if String.length info.body > pipe.maxBytes then
                                        ( partial path world, err "ACKBERRY_LIMIT" "" )

                                    else
                                        ( consumed { world | files = Dict.insert path info.body world.files }
                                        , ok [ ( "bytes", Encode.int (String.length info.body) ), ( "sha256", Encode.string ("sha:" ++ info.body) ) ]
                                        )

            Io.ReadBody read ->
                if socketDead then
                    ( world, err "ECONNRESET" "read" )

                else
                    failOr "read" world <|
                        if String.length info.body > read.maxBytes then
                            ( world, err "ACKBERRY_LIMIT" "" )

                        else
                            ( consumed world, ok [ ( "tag", Encode.string "text" ), ( "text", Encode.string info.body ) ] )

            Io.Sync handle ->
                withFd handle (\_ -> failOr "fsync" world ( world, ok [] ))

            Io.Close handle ->
                withFd handle
                    (\_ ->
                        let
                            closed =
                                { world | fds = Dict.remove (Io.handleToString handle) world.fds }
                        in
                        failOr "close" closed ( closed, ok [] )
                    )

            Io.Stat handle ->
                withFd handle
                    (\path ->
                        failOr "fstat"
                            world
                            ( world, ok [ ( "size", Encode.int (String.length (Maybe.withDefault "" (Dict.get path world.files))) ), ( "isFile", Encode.bool True ) ] )
                    )

            Io.Link link ->
                failOr "link" world <|
                    case ( Dict.get link.from world.files, Dict.member link.to world.files ) of
                        ( _, True ) ->
                            ( world, err "EEXIST" "link" )

                        ( Nothing, False ) ->
                            ( world, err "ENOENT" "link" )

                        ( Just content, False ) ->
                            ( { world | files = Dict.insert link.to content world.files }, ok [] )

            Io.Unlink path ->
                failOr "unlink" world <|
                    if Dict.member path world.files then
                        ( { world | files = Dict.remove path world.files }, ok [] )

                    else
                        ( world, err "ENOENT" "unlink" )

            Io.SyncDir _ ->
                failOr "fsync" world ( world, ok [] )

            Io.HttpGet get ->
                failOr "connect" world <|
                    if String.endsWith "/items/42" get.url then
                        ( world
                        , ok
                            [ ( "status", Encode.int 200 )
                            , ( "body", Encode.object [ ( "tag", Encode.string "text" ), ( "text", Encode.string "{\"id\":42,\"name\":\"Towel\",\"priceCents\":1200}" ) ] )
                            ]
                        )

                    else
                        ( world, ok [ ( "status", Encode.int 404 ), ( "body", Encode.object [ ( "tag", Encode.string "text" ), ( "text", Encode.string "" ) ] ) ] )

            Io.Stream stream ->
                withFd stream.handle
                    (\_ ->
                        if socketDead then
                            ( world, err "ECONNRESET" "write" )

                        else
                            case failure of
                                Just _ ->
                                    ( world, err "ERR_STREAM_PREMATURE_CLOSE" "" )

                                Nothing ->
                                    ( world, ok [ ( "bytes", Encode.int stream.length ) ] )
                    )


violate : String -> World -> World
violate why world =
    { world | violations = world.violations ++ [ why ] }



-- STEPPING


{-| A machine moved: start the meter if a streaming step just began, then
deliver any reports.
-}
transition : String -> ( Tx.Machine, List Tx.Report ) -> World -> World
transition id ( machine, reports ) world =
    let
        before =
            Dict.get id world.machines |> Maybe.andThen Tx.inFlight

        stored =
            if Tx.isSettled machine then
                { world | machines = Dict.remove id world.machines }

            else
                { world | machines = Dict.insert id machine world.machines }

        started =
            case Tx.inFlight machine of
                Just op ->
                    if streaming op && Just op /= before then
                        feed (Progressed (Request.idFromString id) 0 True stored.clock) { stored | moved = Dict.insert id 0 stored.moved }

                    else
                        stored

                Nothing ->
                    stored
    in
    List.foldl (\r w -> feed (Reported (Request.idFromString id) (Tx.encodeReport r)) w) started reports


feed : Msg -> World -> World
feed msg world =
    let
        jobsBefore =
            world.model.jobs

        ( model, effects ) =
            Server.step msg world.model
    in
    List.foldl (judgeEmission jobsBefore) { world | model = model } effects
        |> checkStep


{-| Laws about what Elm decides, checked the moment it decides them.
Bridge-bound effects then wait in the outbox.
-}
judgeEmission : Dict String Server.Job -> Effect -> World -> World
judgeEmission jobsBefore effect world =
    let
        wasWaiting key =
            Dict.get key jobsBefore |> Maybe.map (\job -> job.client == Server.Waiting) |> Maybe.withDefault True

        queued =
            { world | outbox = world.outbox ++ [ effect ] }
    in
    case effect of
        Respond id response connection ->
            let
                key =
                    Request.idToString id

                info =
                    Dict.get key world.requests

                unread =
                    (info |> Maybe.map .declared |> Maybe.withDefault False) && not (Set.member key world.drained)

                wire =
                    Response.toWire connection response
            in
            if unread && connection /= Response.CloseAfter then
                violate (key ++ " answered while its body was unread, without closing the connection") queued

            else if wire.status == 408 && Set.member key world.drained then
                violate (key ++ " was told its body did not arrive, after all of it had") queued

            else
                queued

        Extend id program ->
            if not (wasWaiting (Request.idToString id)) && not (List.isEmpty (Plan.steps program)) then
                violate (Request.idToString id ++ " was extended with new steps after its client stopped waiting") queued

            else
                queued

        Begin _ _ ->
            queued

        Abort _ ->
            queued

        Report id fault ->
            { world | faults = world.faults ++ [ ( Request.idToString id, fault ) ] }


{-| The bridge applies one instruction.
-}
carryOut : Effect -> World -> World
carryOut effect world =
    case effect of
        Respond id response connection ->
            let
                key =
                    Request.idToString id

                wire =
                    Response.toWire connection response
            in
            if Set.member key world.over || Set.member key world.left then
                -- The exchange is gone; the bridge drops the answer.
                { world | over = Set.insert key world.over }

            else
                { world
                    | responses = Dict.update key (\p -> Just (Maybe.withDefault [] p ++ [ wire.status ])) world.responses
                    , over = Set.insert key world.over
                }

        Begin id program ->
            transition (Request.idToString id) (Tx.boot (Plan.steps program) (Plan.continuation program)) world

        Extend id program ->
            let
                key =
                    Request.idToString id

                steps =
                    Plan.steps program

                continuation =
                    Plan.continuation program

                checked =
                    world

                streamAnswer =
                    if
                        List.any
                            (\s ->
                                case s of
                                    Tx.Do (Io.Stream _) _ ->
                                        True

                                    _ ->
                                        False
                            )
                            steps
                    then
                        { checked
                            | responses = Dict.update key (\p -> Just (Maybe.withDefault [] p ++ [ 200 ])) checked.responses
                            , over = Set.insert key checked.over
                        }

                    else
                        checked
            in
            case Dict.get key streamAnswer.machines of
                Just machine ->
                    transition key (Tx.append steps continuation machine) streamAnswer

                Nothing ->
                    streamAnswer

        Abort id ->
            let
                key =
                    Request.idToString id
            in
            case Dict.get key world.machines of
                Just machine ->
                    transition key (Tx.preempt machine) world

                Nothing ->
                    world

        Report _ _ ->
            world


checkStep : World -> World
checkStep world =
    let
        l =
            world.model.ledger

        waiting =
            Dict.values world.model.jobs |> List.filter (\job -> job.client == Server.Waiting) |> List.length

        checks =
            [ ( Dict.values world.responses |> List.any (\rs -> List.length rs > 1)
              , "a request was answered twice"
              )
            , ( l.received /= l.responded + l.abandoned + waiting
              , "ledger does not balance: " ++ Debug.toString l
              )
            ]
    in
    List.foldl
        (\( broken, why ) w ->
            if broken && not (List.member why w.violations) then
                violate why w

            else
                w
        )
        world
        checks


drain : Int -> World -> World
drain fuel world =
    if fuel <= 0 then
        violate "drain did not terminate" world

    else if not (List.isEmpty world.outbox) then
        drain (fuel - 1) (deliver 1 world)

    else
        case inFlight world of
            [] ->
                world

            ( id, _ ) :: _ ->
                drain (fuel - 1) (complete Nothing id world)


simulate : List Choice -> World
simulate choices =
    let
        ran =
            List.foldl apply emptyWorld choices |> drain 5000

        -- Long after everything: every deadline has passed.
        later =
            ran.clock + 100000
    in
    drain 5000 (feed (Tick later) { ran | clock = later })



-- LAWS


orphanPaths : World -> Set String
orphanPaths world =
    world.faults
        |> List.filterMap
            (\( _, fault ) ->
                case fault of
                    Verdict.Orphaned path _ ->
                        Just path

                    _ ->
                        Nothing
            )
        |> Set.fromList


anomalies : World -> List String
anomalies world =
    List.filterMap
        (\( _, fault ) ->
            case fault of
                Verdict.Anomaly detail ->
                    Just detail

                _ ->
                    Nothing
        )
        world.faults


{-| An unclassified host error is only legitimate on a request whose own
operations had a failure injected; anywhere else, one request broke another.
-}
interference : World -> List String
interference world =
    List.filterMap
        (\( id, fault ) ->
            case fault of
                Verdict.Unclassified context _ ->
                    if Set.member id world.injectedInto then
                        Nothing

                    else
                        Just (id ++ ": " ++ context)

                _ ->
                    Nothing
        )
        world.faults


{-| A 201 is backed by a final file holding exactly that request's bytes.
-}
createdButNotStored : World -> List String
createdButNotStored world =
    Dict.toList world.requests
        |> List.filterMap
            (\( id, info ) ->
                case ( Dict.get id world.responses, info.final ) of
                    ( Just [ 201 ], Just final ) ->
                        if Dict.get final world.files == Just info.body then
                            Nothing

                        else
                            Just (id ++ " got 201 but " ++ final ++ " holds " ++ Debug.toString (Dict.get final world.files))

                    _ ->
                        Nothing
            )


{-| A committed file holds one complete body sent for it, never a partial
write and never another resource's bytes.
-}
corruptFinals : World -> List String
corruptFinals world =
    Dict.toList world.files
        |> List.filter (\( path, _ ) -> String.endsWith ".final" path || (String.endsWith ".bin" path && path /= "/d/blob_1.bin"))
        |> List.filter
            (\( path, content ) ->
                not (Dict.values world.requests |> List.any (\i -> i.final == Just path && i.body == content))
            )
        |> List.map Tuple.first


unanswered : World -> List String
unanswered world =
    List.range 0 (world.issued - 1)
        |> List.map requestId
        |> List.filter (\id -> not (Dict.member id world.responses) && not (Set.member id world.left))


quiescent : List Choice -> Expectation
quiescent choices =
    let
        world =
            simulate choices

        tmpFiles =
            Dict.keys world.files |> List.filter (String.endsWith ".tmp") |> Set.fromList
    in
    Expect.all
        [ \w -> Expect.equal [] w.violations
        , \w -> Expect.equal [] (anomalies w)
        , \w -> Expect.equal [] (interference w) |> Expect.onFail "a request failed because of another request's operations"
        , \w -> Expect.equal [] (Dict.keys w.machines) |> Expect.onFail "a transaction never settled"
        , \w -> Expect.equal Dict.empty w.fds |> Expect.onFail "an fd (lease) leaked"
        , \w -> Expect.equal 0 (Dict.size w.model.jobs) |> Expect.onFail "a job outlived its transaction"
        , \w -> Expect.equal (orphanPaths w) tmpFiles |> Expect.onFail "temp files on disk differ from the orphans reported"
        , \w -> Expect.equal [] (createdButNotStored w)
        , \w -> Expect.equal [] (corruptFinals w)
        , \w -> Expect.equal [] (unanswered w) |> Expect.onFail "a request was neither answered nor abandoned"
        , \w -> Expect.equal w.model.ledger.received (w.model.ledger.responded + w.model.ledger.abandoned)
        ]
        world



-- COVERAGE GUARDS


answeredWith : Int -> World -> Bool
answeredWith code world =
    Dict.values world.responses |> List.concat |> List.member code


uploadCommitted : World -> Bool
uploadCommitted world =
    Dict.member "/d/blob_3.bin" world.files


abortedMidBody : World -> Bool
abortedMidBody world =
    Dict.toList world.requests
        |> List.any (\( id, info ) -> info.final /= Nothing && Set.member id world.left && not (Dict.member id world.responses))


suite : Test
suite =
    describe "Server against a hostile simulated host"
        [ fuzzWith
            { runs = 1500
            , distribution =
                Test.expectDistribution
                    [ ( Test.Distribution.atLeast 15, "a commit answered 201", simulate >> answeredWith 201 )
                    , ( Test.Distribution.atLeast 10, "a streamed upload committed", simulate >> uploadCommitted )
                    , ( Test.Distribution.atLeast 10, "a lease race lost (409)", simulate >> answeredWith 409 )
                    , ( Test.Distribution.atLeast 5, "a client vanished mid-body", simulate >> abortedMidBody )
                    , ( Test.Distribution.atLeast 3, "the meter answered 408", simulate >> answeredWith 408 )
                    , ( Test.Distribution.atLeast 3, "a body was refused for size (413)", simulate >> answeredWith 413 )
                    , ( Test.Distribution.atLeast 2, "a temp file was orphaned and reported", simulate >> orphanPaths >> Set.isEmpty >> not )
                    ]
            }
            (Fuzz.listOfLengthBetween 10 90 choice)
            "no leaked fds, no unreported temp files, no 201 without its bytes, no hung request"
            quiescent
        ]
