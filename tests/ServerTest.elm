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
import Plan
import Server exposing (Effect(..), Msg(..))
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
    }


a : Request.RequestId
a =
    Request.idFromString "a"



-- SIMULATION


type alias Sim =
    { model : Server.Model
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
    feed (Received a 0 (wire method target headers)) { model = Server.init config, machine = Nothing, effects = [], performed = [] }


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
        Begin _ program ->
            run (Tx.boot (Plan.steps program) (Plan.continuation program)) sim

        Extend _ program ->
            onMachine (Tx.append (Plan.steps program) (Plan.continuation program)) sim

        Abort _ ->
            onMachine Tx.preempt sim

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
    List.foldl (\r s -> feed (Reported a (Tx.encodeReport r)) s) { sim | machine = Just machine, performed = started } reports


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

                _ ->
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
commitOps fill tmp final =
    [ "open " ++ tmp, fill, "fsync", "link " ++ final, "unlink " ++ tmp, "fsync-dir /d", "close" ]



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
                            [ names >> Expect.equal ("read-body" :: commitOps "write" "/d/file_42.tmp" "/d/file_42.final")
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
                            , names >> Expect.equal [ "read-body", "open /d/file_42.tmp" ]
                            , settled
                            ]
            , test "EEXIST on the lease is 409, and the holder's temp file is never touched" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (failing isOpen "EEXIST" "open" payload)
                        |> Expect.all
                            [ one (statusIs 409)
                            , names >> List.filter (String.startsWith "unlink") >> Expect.equal []
                            ]
            , test "a link that finds the final file is 409, and the rollback removes only our temp file" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (failing isLink "EEXIST" "link" payload)
                        |> Expect.all
                            [ one (statusIs 409)
                            , names >> List.drop 4 >> Expect.equal [ "link /d/file_42.final", "unlink /d/file_42.tmp", "close" ]
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
            [ test "PUT streams the socket into the leased temp file and commits it" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answerAll (host "")
                        |> Expect.all
                            [ names >> Expect.equal (commitOps "pipe-body" "/d/blob_9.tmp" "/d/blob_9.bin")
                            , one (\w -> Expect.equal ( 201, "{\"blobId\":9,\"bytes\":10485760,\"sha256\":\"f00d\"}" ) ( w.status, w.body ))
                            , settled
                            ]
            , test "a declared length over the limit is refused from the headers: no transaction at all" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10737418240" ) ]
                        |> Expect.all
                            [ one (statusIs 413)
                            , answers >> List.map Tuple.second >> Expect.equal [ Response.CloseAfter ]
                            , kinds >> Expect.equal [ "respond" ]
                            ]
            , test "the client leaves mid-body: abort, the pipe is severed, unlink then close, no answer" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> feed (ClientGone a)
                        |> answerAll (host "")
                        |> Expect.all
                            [ names >> Expect.equal [ "open /d/blob_9.tmp", "pipe-body", "unlink /d/blob_9.tmp", "close" ]
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
                            , answers >> List.map Tuple.second >> Expect.equal [ Response.CloseAfter ]
                            , names >> Expect.equal [ "open /d/blob_9.tmp", "pipe-body", "unlink /d/blob_9.tmp", "close" ]
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
                            [ names >> Expect.equal (commitOps "pipe-body" "/d/blob_9.tmp" "/d/blob_9.bin")
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
                            [ names >> Expect.equal [ "open /d/blob_1.bin", "stat", "stream", "close" ]
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
                            , names >> Expect.equal [ "open /d/blob_1.bin", "stat", "stream", "close" ]
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
                            [ one (statusIs 500), names >> Expect.equal [ "open /d/blob_7.bin", "stat", "close" ], settled ]
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
                        |> Expect.equal [ ( 404, Response.CloseAfter ) ]
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
        ]
