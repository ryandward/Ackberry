module ServerTest exposing (suite)

{-| Server contracts, with the reference interpreter standing in for the
bridge: Begin/Extend/Abort effects drive a `Tx.Machine`, and its reports go
back into `Server.step` as the bridge would send them.
-}

import Dict
import Expect exposing (Expectation)
import Http.Request as Request
import Http.Response as Response
import Io exposing (HostError, Op)
import Json.Decode as Decode
import Json.Encode as Encode
import Layout
import Plan
import Server exposing (Effect(..), Msg(..), Owner(..))
import Test exposing (Test, describe, test)
import Tx
import Verdict


config : Server.Config
config =
    { dataDir = "/d"
    , upstream = "http://up"
    , deadlineMs = 2000
    , transferDeadlineMs = 60000
    , transferRate = { bytes = 4096, perMs = 1000 }
    , deliveryRate = { bytes = 4096, perMs = 1000 }
    , jsonLimit = 1048576
    , uploadLimit = 104857600
    , progressEveryMs = 100
    , staleAfterMs = 7200000
    , sweepEveryMs = 900000
    , linger = { deadlineMs = 500, maxBytes = 131072, maxSockets = 64 }
    }


a : Request.RequestId
a =
    Request.idFromString "a"



-- SIMULATION


{-| One owner's transactions, run on the reference machine. Effects for
other owners are recorded but not run.
-}
type alias Sim =
    { model : Server.Model
    , owner : Owner
    , machine : Maybe Tx.Machine
    , effects : List Effect
    , performed : List Op
    }


wire : String -> String -> List ( String, String ) -> Encode.Value
wire method target headers =
    Encode.object
        [ ( "method", Encode.string method )
        , ( "target", Encode.string target )
        , ( "headers", Encode.list Encode.string (List.concatMap (\( n, v ) -> [ n, v ]) (( "Host", "x" ) :: headers)) )
        ]


arrive : String -> String -> List ( String, String ) -> Sim
arrive method target headers =
    feed (Received a 0 (wire method target headers))
        { model = Tuple.first (Server.init config), owner = Exchange a, machine = Nothing, effects = [], performed = [] }


{-| A server just started, following its janitor.
-}
booting : Server.Config -> Sim
booting cfg =
    let
        ( model, effects ) =
            Server.init cfg
    in
    List.foldl applyEffect { model = model, owner = Janitor, machine = Nothing, effects = effects, performed = [] } effects


withBody : String -> List ( String, String )
withBody body =
    [ ( "Content-Length", String.fromInt (String.length body) ) ]


feed : Msg -> Sim -> Sim
feed msg sim =
    let
        ( model, effects ) =
            Server.step msg sim.model
    in
    List.foldl applyEffect { sim | model = model, effects = sim.effects ++ effects } effects


applyEffect : Effect -> Sim -> Sim
applyEffect effect sim =
    case effect of
        Begin owner program ->
            if owner == sim.owner then
                run (Tx.boot (Plan.steps program) (Plan.continuation program)) sim

            else
                sim

        Extend owner program ->
            if owner == sim.owner then
                onMachine (Tx.append (Plan.steps program) (Plan.continuation program)) sim

            else
                sim

        Abort id ->
            if Exchange id == sim.owner then
                onMachine Tx.preempt sim

            else
                sim

        _ ->
            sim


onMachine : (Tx.Machine -> ( Tx.Machine, List Tx.Report )) -> Sim -> Sim
onMachine f sim =
    case sim.machine of
        Just machine ->
            run (f machine) sim

        Nothing ->
            sim


run : ( Tx.Machine, List Tx.Report ) -> Sim -> Sim
run ( machine, reports ) sim =
    let
        started =
            case ( Tx.inFlight machine, sim.machine |> Maybe.andThen Tx.inFlight ) of
                ( Just op, _ ) ->
                    if Just op /= (sim.machine |> Maybe.andThen Tx.inFlight) || sim.machine == Nothing then
                        sim.performed ++ [ op ]

                    else
                        sim.performed

                ( Nothing, _ ) ->
                    sim.performed
    in
    List.foldl (\r s -> feed (Reported s.owner (Tx.encodeReport r)) s) { sim | machine = Just machine, performed = started } reports


{-| The host finishes whatever is in flight. A signalled interruptible op
ends as the bridge's drivers end it: aborted.
-}
answer : (Op -> Result HostError Decode.Value) -> Sim -> Sim
answer reply sim =
    case sim.machine of
        Just machine ->
            case Tx.inFlight machine of
                Just op ->
                    run
                        (Tx.land
                            (if Tx.signalled machine && Io.interruptible op then
                                Err Io.revokedError

                             else
                                reply op
                            )
                            machine
                        )
                        sim

                Nothing ->
                    sim

        Nothing ->
            sim


answerAll : (Op -> Result HostError Decode.Value) -> Sim -> Sim
answerAll reply sim =
    List.foldl (\_ s -> answer reply s) sim (List.range 1 20)



-- HOSTS


host : String -> Op -> Result HostError Decode.Value
host body op =
    let
        ok fields =
            Ok (Encode.object fields)
    in
    case op of
        Io.ReadBody _ ->
            ok [ ( "tag", Encode.string "text" ), ( "text", Encode.string body ) ]

        Io.Write _ text ->
            ok [ ( "bytes", Encode.int (String.length text) ), ( "sha256", Encode.string "abc123" ) ]

        Io.PipeBody _ ->
            ok [ ( "bytes", Encode.int 10485760 ), ( "sha256", Encode.string "f00d" ) ]

        Io.Stat _ ->
            ok [ ( "size", Encode.int 10485760 ), ( "isFile", Encode.bool True ) ]

        Io.HttpGet _ ->
            ok
                [ ( "status", Encode.int 200 )
                , ( "body", Encode.object [ ( "tag", Encode.string "text" ), ( "text", Encode.string "{\"id\":42,\"name\":\"Towel\",\"priceCents\":1200}" ) ] )
                ]

        Io.Stream _ ->
            ok [ ( "bytes", Encode.int 10485760 ) ]

        _ ->
            ok []


hostError : String -> String -> HostError
hostError code syscall =
    Decode.decodeValue Io.hostErrorDecoder
        (Encode.object
            [ ( "name", Encode.string "Error" )
            , ( "code", Encode.string code )
            , ( "syscall", Encode.string syscall )
            , ( "rawMessage", Encode.string (code ++ ": /secret/server/path") )
            , ( "cause", Encode.null )
            ]
        )
        |> Result.withDefault Io.revokedError


{-| `host`, except operations matching `which` fail with `code`.
-}
failing : (Op -> Bool) -> String -> String -> String -> Op -> Result HostError Decode.Value
failing which code syscall body op =
    if which op then
        Err (hostError code syscall)

    else
        host body op


isOpen : Op -> Bool
isOpen op =
    case op of
        Io.Open _ ->
            True

        _ ->
            False


isLink : Op -> Bool
isLink op =
    case op of
        Io.Link _ ->
            True

        _ ->
            False


isUnlink : Op -> Bool
isUnlink op =
    case op of
        Io.Unlink _ ->
            True

        _ ->
            False


isEnsureDir : Op -> Bool
isEnsureDir op =
    case op of
        Io.EnsureDir _ ->
            True

        _ ->
            False


{-| What the janitor's host shows it: a listing taken at `at`, and unlinks
that fail where `unlinkFails` says so.
-}
type Listed
    = File String Float
    | Directory String Float
    | Unreadable String String


janitorHost : Int -> List Listed -> (String -> Maybe String) -> Op -> Result HostError Decode.Value
janitorHost at listed unlinkFails op =
    let
        meta isFile mtime =
            Encode.object [ ( "ok", Encode.object [ ( "isFile", Encode.bool isFile ), ( "mtimeMs", Encode.float mtime ) ] ) ]

        entry item =
            case item of
                File name mtime ->
                    Encode.object [ ( "name", Encode.string name ), ( "stat", meta True mtime ) ]

                Directory name mtime ->
                    Encode.object [ ( "name", Encode.string name ), ( "stat", meta False mtime ) ]

                Unreadable name code ->
                    Encode.object [ ( "name", Encode.string name ), ( "stat", Encode.object [ ( "error", Io.encodeHostError (hostError code "lstat") ) ] ) ]
    in
    case op of
        Io.ListDir _ ->
            Ok (Encode.object [ ( "at", Encode.int at ), ( "entries", Encode.list entry listed ) ])

        Io.UnlinkEach paths ->
            Ok
                (Encode.object
                    [ ( "outcomes"
                      , Encode.list
                            (\path ->
                                case unlinkFails path of
                                    Nothing ->
                                        Encode.object [ ( "ok", Encode.object [] ) ]

                                    Just code ->
                                        Encode.object [ ( "error", Io.encodeHostError (hostError code "unlink") ) ]
                            )
                            paths
                      )
                    ]
                )

        _ ->
            Ok (Encode.object [])


{-| The paths the janitor was told to remove.
-}
doomed : Sim -> List String
doomed sim =
    List.concatMap
        (\op ->
            case op of
                Io.UnlinkEach paths ->
                    paths

                _ ->
                    []
        )
        sim.performed



-- OBSERVATIONS


answers : Sim -> List ( Response.Wire, Response.Connection )
answers sim =
    List.filterMap
        (\effect ->
            case effect of
                Respond _ response connection ->
                    Just ( Response.toWire connection response, connection )

                _ ->
                    Nothing
        )
        sim.effects


one : (Response.Wire -> Expectation) -> Sim -> Expectation
one check sim =
    case answers sim of
        [ ( w, _ ) ] ->
            check w

        out ->
            Expect.fail ("expected exactly one response, got " ++ Debug.toString (List.map (Tuple.first >> .status) out))


statusIs : Int -> Response.Wire -> Expectation
statusIs code w =
    Expect.equal code w.status |> Expect.onFail w.body


names : Sim -> List String
names sim =
    List.map opName sim.performed


opName : Op -> String
opName op =
    case op of
        Io.Open open ->
            "open " ++ open.path

        Io.Write _ _ ->
            "write"

        Io.Sync _ ->
            "fsync"

        Io.Close _ ->
            "close"

        Io.Stat _ ->
            "stat"

        Io.Link link ->
            "link " ++ link.to

        Io.Unlink path ->
            "unlink " ++ path

        Io.SyncDir path ->
            "fsync-dir " ++ path

        Io.HttpGet _ ->
            "get"

        Io.ReadBody _ ->
            "read-body"

        Io.PipeBody _ ->
            "pipe-body"

        Io.Stream _ ->
            "stream"

        Io.EnsureDir path ->
            "ensure-dir " ++ path

        Io.ListDir path ->
            "list-dir " ++ path

        Io.UnlinkEach paths ->
            "unlink-each " ++ String.join "," paths


faults : Sim -> List Verdict.Fault
faults sim =
    List.filterMap
        (\effect ->
            case effect of
                Report _ fault ->
                    Just fault

                _ ->
                    Nothing
        )
        sim.effects


kinds : Sim -> List String
kinds sim =
    List.filterMap
        (\effect ->
            case effect of
                Respond _ _ _ ->
                    Just "respond"

                Abort _ ->
                    Just "abort"

                Extend _ _ ->
                    Just "extend"

                Begin _ _ ->
                    Just "begin"

                Listen ->
                    Just "listen"

                Halt _ ->
                    Just "halt"

                Report _ _ ->
                    Nothing
        )
        sim.effects


settled : Sim -> Expectation
settled sim =
    Expect.equal 0 (Dict.size sim.model.jobs) |> Expect.onFail "a job outlived its transaction"


payload : String
payload =
    "{\"hello\":\"world\"}"


commitOps : String -> String -> String -> List String
commitOps fill final dir =
    [ "open /d/.tmp/a.part", fill, "fsync", "link " ++ final, "unlink /d/.tmp/a.part", "fsync-dir " ++ dir, "close" ]



-- TESTS


suite : Test
suite =
    describe "Server.step"
        [ describe "buffered ingest"
            [ test "the happy path: read the body, pause, then one commit transaction, 201 last" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (host payload)
                        |> Expect.all
                            [ names >> Expect.equal ("read-body" :: commitOps "write" "/d/file_42.final" "/d")
                            , one (\w -> Expect.equal ( 201, "{\"itemId\":42,\"bytes\":17,\"sha256\":\"abc123\"}" ) ( w.status, w.body ))
                            , answers >> List.map Tuple.second >> Expect.equal [ Response.KeepAlive ]
                            , settled
                            ]
            , test "poison disk: EACCES on the lease is a 500 naming the host error, without server paths" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (failing isOpen "EACCES" "open" payload)
                        |> Expect.all
                            [ one
                                (\w ->
                                    Expect.equal ( 500, True, False )
                                        ( w.status
                                        , String.contains "\"hostError\":{\"code\":\"EACCES\",\"syscall\":\"open\"}" w.body
                                        , String.contains "/secret" w.body
                                        )
                                )
                            , names >> Expect.equal [ "read-body", "open /d/.tmp/a.part" ]
                            , settled
                            ]
            , test "EEXIST on a fresh part is a broken invariant, not a conflict: 500, an anomaly, and the file is never touched" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (failing isOpen "EEXIST" "open" payload)
                        |> Expect.all
                            [ one (statusIs 500)
                            , faults >> List.length >> Expect.equal 1
                            , names >> List.filter (String.startsWith "unlink") >> Expect.equal []
                            ]
            , test "a link that finds the final file is 409, and the rollback removes only our temp file" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (failing isLink "EEXIST" "link" payload)
                        |> Expect.all
                            [ one (statusIs 409)
                            , names >> List.drop 4 >> Expect.equal [ "link /d/file_42.final", "unlink /d/.tmp/a.part", "close" ]
                            , settled
                            ]
            , test "a rollback whose unlink is denied reports the orphan" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (\op -> failing (\o -> isLink o || isUnlink o) "EACCES" "x" payload op)
                        |> Expect.all
                            [ one (statusIs 500)
                            , \sim -> Expect.equal 1 sim.model.ledger.orphans
                            , settled
                            ]
            , test "a non-object payload is refused after the read, and the paused transaction just finishes" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody "[1,2]")
                        |> answerAll (host "[1,2]")
                        |> Expect.all [ one (statusIs 400), names >> Expect.equal [ "read-body" ], settled ]
            ]
        , describe "streamed upload"
            [ test "PUT streams the socket into the transaction's own part and links it to the blob's name" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answerAll (host "")
                        |> Expect.all
                            [ names >> Expect.equal (commitOps "pipe-body" "/d/blobs/9" "/d/blobs")
                            , one (\w -> Expect.equal ( 201, "{\"blobId\":9,\"bytes\":10485760,\"sha256\":\"f00d\"}" ) ( w.status, w.body ))
                            , settled
                            ]
            , test "a declared length over the limit is refused from the headers: no transaction at all" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10737418240" ) ]
                        |> Expect.all
                            [ one (statusIs 413)
                            , answers >> List.map Tuple.second >> Expect.equal [ Response.CloseAfter config.linger ]
                            , kinds >> Expect.equal [ "respond" ]
                            ]
            , test "the client leaves mid-body: abort, the pipe is severed, unlink then close, no answer" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> feed (ClientGone a)
                        |> answerAll (host "")
                        |> Expect.all
                            [ names >> Expect.equal [ "open /d/.tmp/a.part", "pipe-body", "unlink /d/.tmp/a.part", "close" ]
                            , answers >> Expect.equal []
                            , kinds >> Expect.equal [ "begin", "abort" ]
                            , \sim -> Expect.equal 1 sim.model.ledger.abandoned
                            , settled
                            ]
            , test "a slowloris body: the meter answers 408 and aborts, in that order, then the rollback runs" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> feed (Progressed a 0 True 0)
                        |> feed (Progressed a 300 True 500)
                        |> feed (Tick 1100)
                        |> answerAll (host "")
                        |> Expect.all
                            [ kinds >> Expect.equal [ "begin", "respond", "abort" ]
                            , one (statusIs 408)
                            , answers >> List.map Tuple.second >> Expect.equal [ Response.CloseAfter config.linger ]
                            , names >> Expect.equal [ "open /d/.tmp/a.part", "pipe-body", "unlink /d/.tmp/a.part", "close" ]
                            , \sim -> Expect.equal 1 sim.model.ledger.tooSlow
                            , settled
                            ]
            , test "a body that keeps pace is left alone and the meter moves its bar" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> feed (Progressed a 0 True 0)
                        |> feed (Progressed a 9000 True 300)
                        |> feed (Tick 1100)
                        |> Expect.all
                            [ kinds >> Expect.equal [ "begin" ]
                            , \sim ->
                                Dict.values sim.model.jobs
                                    |> List.filterMap .meter
                                    |> List.map (\m -> ( m.checkAt, m.required ))
                                    |> Expect.equal [ ( 2100, 9000 + 4096 ) ]
                            ]
            , test "a count the bridge may not have reported yet is never judged: the window waits out the validity bound" <|
                \_ ->
                    -- 3000 bytes reported at 10 ms; 3000 more arrived inside the
                    -- throttle and are reported by the trailing report at 110 ms.
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> feed (Progressed a 0 True 0)
                        |> feed (Progressed a 3000 True 10)
                        |> feed (Tick 1000)
                        |> feed (Progressed a 6000 True 110)
                        |> feed (Tick 1100)
                        |> kinds
                        |> Expect.equal [ "begin" ]
            , test "the window opens when the bridge says the step started, not when Elm last read its clock" <|
                \_ ->
                    -- Elm's clock says 500; the bridge started the body step at 900.
                    -- The first verdict is due at 900 + 1000 + 100, not before.
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> feed (Tick 500)
                        |> answer (host "")
                        |> feed (Progressed a 0 True 900)
                        |> feed (Tick 1700)
                        |> kinds
                        |> Expect.equal [ "begin" ]
            , test "a slow disk after the body arrived is never blamed on the client: stale rate checks do nothing" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "100" ) ]
                        |> answer (host "")
                        |> feed (Progressed a 0 True 0)
                        |> feed (Progressed a 100 False 50)
                        |> feed (Tick 1100)
                        |> kinds
                        |> Expect.equal [ "begin" ]
            , test "a client that leaves after the commit point: the commit rolls forward, nobody is answered" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> answer (host "")
                        |> answer (host "")
                        |> answer (host "")
                        |> feed (ClientGone a)
                        |> answerAll (host "")
                        |> Expect.all
                            [ names >> Expect.equal (commitOps "pipe-body" "/d/blobs/9" "/d/blobs")
                            , answers >> Expect.equal []
                            , settled
                            ]
            ]
        , describe "downloads"
            [ test "open, stat, pause; then the stream is the answer and close is the Always cleanup" <|
                \_ ->
                    arrive "GET" "/blobs/1" []
                        |> answerAll (host "")
                        |> Expect.all
                            [ names >> Expect.equal [ "open /d/blobs/1", "stat", "stream", "close" ]
                            , answers >> Expect.equal []
                            , \sim -> Expect.equal 1 sim.model.ledger.responded
                            , settled
                            ]
            , test "a slow reader is aborted by the delivery meter and its fd still closes" <|
                \_ ->
                    arrive "GET" "/blobs/1" []
                        |> answer (host "")
                        |> answer (host "")
                        |> feed (Progressed a 0 True 0)
                        |> feed (Tick 1100)
                        |> answerAll (host "")
                        |> Expect.all
                            [ kinds >> Expect.equal [ "begin", "extend", "abort" ]
                            , names >> Expect.equal [ "open /d/blobs/1", "stat", "stream", "close" ]
                            , settled
                            ]
            , test "ENOENT is the only error that means missing: 404" <|
                \_ -> arrive "GET" "/blobs/2" [] |> answerAll (failing isOpen "ENOENT" "open" "") |> one (statusIs 404)
            , test "ELOOP is not missing data: fatal 500 and a logged fault" <|
                \_ ->
                    arrive "GET" "/blobs/9" []
                        |> answerAll (failing isOpen "ELOOP" "open" "")
                        |> Expect.all
                            [ one (\w -> Expect.equal ( 500, True ) ( w.status, String.contains "\"fatal\":true" w.body ))
                            , faults >> List.length >> Expect.equal 1
                            ]
            , test "a FIFO or directory is closed again and answered 500" <|
                \_ ->
                    arrive "GET" "/blobs/7" []
                        |> answerAll
                            (\op ->
                                case op of
                                    Io.Stat _ ->
                                        Ok (Encode.object [ ( "size", Encode.int 0 ), ( "isFile", Encode.bool False ) ])

                                    _ ->
                                        host "" op
                            )
                        |> Expect.all
                            [ one (statusIs 500), names >> Expect.equal [ "open /d/blobs/7", "stat", "close" ], settled ]
            ]
        , describe "upstream"
            [ test "the deadline answers 504 and aborts; the severed call answers nobody" <|
                \_ ->
                    arrive "GET" "/delayed/42" []
                        |> feed (Tick 2000)
                        |> answerAll (host "")
                        |> Expect.all [ one (statusIs 504), kinds >> Expect.equal [ "begin", "respond", "abort" ], settled ]
            , test "ECONNREFUSED is a 502" <|
                \_ -> arrive "GET" "/delayed/42" [] |> answerAll (failing (always True) "ECONNREFUSED" "connect" "") |> one (statusIs 502)
            , test "a cancellation nobody asked for still answers the waiting client" <|
                \_ ->
                    arrive "GET" "/delayed/42" []
                        |> answerAll (failing (always True) "ABORT_ERR" "" "")
                        |> Expect.all
                            [ one (statusIs 500)
                            , faults >> List.length >> Expect.equal 1
                            , settled
                            ]
            ]
        , describe "bodies Elm never read"
            [ test "a POST to an unknown path gets 404 and the connection closes instead of draining the body" <|
                \_ ->
                    arrive "POST" "/nope" (withBody "xxxx")
                        |> answers
                        |> List.map (\( w, c ) -> ( w.status, c ))
                        |> Expect.equal [ ( 404, Response.CloseAfter config.linger ) ]
            , test "a slow JSON body is answered 408 by the meter" <|
                \_ ->
                    arrive "POST" "/echo" (withBody payload)
                        |> feed (Progressed a 0 True 0)
                        |> feed (Tick 1100)
                        |> answerAll (host payload)
                        |> Expect.all [ one (statusIs 408), settled ]
            , test "echo reflects the exact text" <|
                \_ ->
                    let
                        sent =
                            "{\"n\":12345678901234567890,\"b\":1,\"b\":2}"
                    in
                    arrive "POST" "/echo" (withBody sent) |> answerAll (host sent) |> one (\w -> Expect.equal ( 200, sent ) ( w.status, w.body ))
            , test "an unknown method is 501 and 405 carries Allow" <|
                \_ ->
                    Expect.equal ( [ 501 ], [ 405 ] )
                        ( arrive "PURGE" "/health" [] |> answers |> List.map (Tuple.first >> .status)
                        , arrive "PUT" "/health" [] |> answers |> List.map (Tuple.first >> .status)
                        )
            ]
        , describe "refused connections"
            [ test "a release is counted and decides nothing" <|
                \_ ->
                    arrive "GET" "/health" []
                        |> feed (Released a (Encode.object [ ( "how", Encode.string "budget" ), ( "discarded", Encode.int 196608 ) ]))
                        |> feed (Released a (Encode.object [ ( "how", Encode.string "hungUp" ), ( "discarded", Encode.int 1000 ) ]))
                        |> feed (Released a (Encode.object [ ( "how", Encode.string "poolFull" ), ( "discarded", Encode.int 0 ) ]))
                        |> Expect.all
                            [ \sim ->
                                Expect.equal { hungUp = 1, deadlinePassed = 0, budgetSpent = 1, poolFull = 1, bytesDiscarded = 197608 }
                                    sim.model.ledger.lingered
                            , kinds >> Expect.equal [ "respond" ]
                            ]
            , test "a release that does not decode is an anomaly, not a guess" <|
                \_ ->
                    arrive "GET" "/health" []
                        |> feed (Released a (Encode.object [ ( "how", Encode.string "evaporated" ), ( "discarded", Encode.int 0 ) ]))
                        |> Expect.all
                            [ faults >> List.length >> Expect.equal 1
                            , \sim -> Expect.equal 0 sim.model.ledger.lingered.hungUp
                            ]
            , test "negative linger bounds refuse to start" <|
                \_ ->
                    Server.init { config | linger = { deadlineMs = 500, maxBytes = -1, maxSockets = 64 } }
                        |> Tuple.second
                        |> List.map
                            (\effect ->
                                case effect of
                                    Halt _ ->
                                        "halt"

                                    _ ->
                                        "other"
                            )
                        |> Expect.equal [ "halt" ]
            ]
        , describe "the janitor"
            [ test "the server opens only once its layout exists: prepare, then Listen and a survey" <|
                \_ ->
                    booting config
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> Expect.all
                            [ names >> Expect.equal [ "ensure-dir /d/.tmp", "ensure-dir /d/blobs", "list-dir /d/.tmp" ]
                            , kinds >> Expect.equal [ "begin", "listen", "begin" ]
                            ]
            , test "a layout that cannot be prepared halts: no Listen, no sweep, no alarm, the host error logged" <|
                \_ ->
                    booting config
                        |> answerAll (failing isEnsureDir "EACCES" "mkdir" "")
                        |> Expect.all
                            [ kinds >> Expect.equal [ "begin", "halt" ]
                            , faults >> List.length >> Expect.equal 1
                            , \sim -> Expect.equal Nothing (Server.sweepEvery sim.model)
                            ]
            , test "a server closed while it boots never opens" <|
                \_ ->
                    booting config
                        |> feed Closed
                        |> answerAll (janitorHost 0 [] (always Nothing))
                        |> Expect.all
                            [ kinds >> Expect.equal [ "begin" ]
                            , \sim -> Expect.equal Nothing (Server.sweepEvery sim.model)
                            ]
            , test "a staleAfterMs no longer than the transfer deadline refuses to start" <|
                \_ ->
                    Server.init { config | staleAfterMs = config.transferDeadlineMs }
                        |> Tuple.second
                        |> List.map
                            (\effect ->
                                case effect of
                                    Halt _ ->
                                        "halt"

                                    _ ->
                                        "other"
                            )
                        |> Expect.equal [ "halt" ]
            , test "a sweep removes exactly the stale parts, and only regular files named like parts" <|
                \_ ->
                    let
                        now =
                            10000000

                        listing =
                            [ File "old.part" 1000000
                            , File "young.part" 9000000
                            , File "notes.txt" 0
                            , Directory "dir.part" 0
                            , File ".part" 0
                            , Unreadable "gone.part" "ENOENT"
                            , Unreadable "sick.part" "EIO"
                            ]
                    in
                    booting config
                        |> answerAll (janitorHost now listing (always Nothing))
                        |> Expect.all
                            [ doomed >> Expect.equal [ "/d/.tmp/old.part" ]
                            , faults >> List.length >> Expect.equal 1 >> Expect.onFail "the unreadable part, and only it, is reported"
                            , \sim -> Expect.equal 1 sim.model.ledger.swept
                            , \sim -> Expect.equal (Just config.sweepEveryMs) (Server.sweepEvery sim.model)
                            ]
            , test "staleness is judged by the host clock at the listing, never by Elm's clock" <|
                \_ ->
                    -- Elm last read its clock far in the future; the host
                    -- listed at 10 000 000. By Elm's clock young.part would
                    -- look ancient.
                    booting config
                        |> feed (Tick 99999999999)
                        |> answerAll (janitorHost 10000000 [ File "young.part" 9000000 ] (always Nothing))
                        |> doomed
                        |> Expect.equal []
            , test "a part held by a live upload is never taken, even when its mtime says stale" <|
                \_ ->
                    booting config
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> feed (Received a 0 (wire "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]))
                        |> answerAll (janitorHost 99999999 [ File "a.part" 0, File "dead.part" 0 ] (always Nothing))
                        |> doomed
                        |> Expect.equal [ "/d/.tmp/dead.part" ]
            , test "an unlink that fails is an orphan, one that finds nothing is not; the rest still go" <|
                \_ ->
                    booting config
                        |> answerAll
                            (janitorHost 99999999
                                [ File "a1.part" 0, File "a2.part" 0, File "a3.part" 0 ]
                                (\path ->
                                    if path == "/d/.tmp/a2.part" then
                                        Just "EACCES"

                                    else if path == "/d/.tmp/a3.part" then
                                        Just "ENOENT"

                                    else
                                        Nothing
                                )
                            )
                        |> Expect.all
                            [ doomed >> Expect.equal [ "/d/.tmp/a1.part", "/d/.tmp/a2.part", "/d/.tmp/a3.part" ]
                            , \sim -> Expect.equal ( 1, 1 ) ( sim.model.ledger.swept, sim.model.ledger.orphans )
                            ]
            , test "the alarm starts a sweep only between sweeps, and a listing that fails is reported and retried later" <|
                \_ ->
                    booting config
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> feed SweepDue
                        |> answerAll (failing (always True) "EIO" "scandir" "")
                        |> feed SweepDue
                        |> Expect.all
                            [ kinds >> Expect.equal [ "begin", "listen", "begin", "begin" ]
                            , faults >> List.length >> Expect.equal 1
                            ]
            ]
        , describe "layout"
            [ test "partFromName inverts part for a UUID, and refuses names part could not have made" <|
                \_ ->
                    let
                        layout =
                            Layout.fromDataDir "/d"

                        uuid =
                            "0f8fad5b-d9cb-469f-a165-70867728950e"
                    in
                    Expect.equal
                        ( Just (Layout.part layout (Request.idFromString uuid)), [ Nothing, Nothing, Nothing, Nothing ] )
                        ( Layout.partFromName layout (uuid ++ ".part")
                        , List.map (Layout.partFromName layout) [ uuid ++ ".tmp", ".part", "notes", "blob_9.tmp" ]
                        )
            , test "a part is named after its transaction, never after the item it will become" <|
                \_ ->
                    Layout.partPath (Layout.part (Layout.fromDataDir "/d") (Request.idFromString "r7"))
                        |> Expect.equal "/d/.tmp/r7.part"
            ]
        ]
