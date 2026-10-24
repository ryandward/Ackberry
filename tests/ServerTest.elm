module ServerTest exposing (suite)

{-| Server contracts, with the reference interpreter standing in for the
bridge: Begin/Extend/Abort effects drive a `Tx.Machine`, and its reports go
back into `Server.step` as the bridge would send them. Requests arrive as
heads on a connection; answers are read back from the bytes Elm serialized.
-}

import Dict
import Expect exposing (Expectation)
import Http.Request as Request
import Io exposing (HostError, Op)
import Json.Decode as Decode
import Json.Encode as Encode
import Kernel
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
    , head = { maxHeadBytes = 16384, maxTarget = 8192, maxFields = 100 }
    , headDeadlineMs = 10000
    , idleMs = 5000
    , tickMs = 100
    , linger = { deadlineMs = 500, budgetBytes = 131072, maxSockets = 64 }
    , chunked = { maxLine = 8192, maxTrailer = 16384 }
    }


conn : Request.ConnId
conn =
    Request.connIdFromString "c"


a : Request.RequestId
a =
    Request.idWithin conn 0


clockAt : Int -> Server.Clock
clockAt mono =
    { mono = mono, wall = 784111777000 + mono }


crlf : String
crlf =
    "\u{000D}\n"



-- SIMULATION


type alias Sim =
    { model : Server.Model
    , owner : Owner
    , machine : Maybe Tx.Machine
    , effects : List Effect
    , performed : List Op
    , now : Int
    }


{-| A server past its boot, its janitor resting until far away.
-}
serving : Server.Config -> Server.Model
serving cfg =
    let
        m =
            Tuple.first (Server.init cfg)
    in
    { m | phase = Server.Serving, sweep = Server.Idle, nextSweepAt = 1000000000 }


headText : String -> String -> List ( String, String ) -> String
headText method target headers =
    String.join crlf ((method ++ " " ++ target ++ " HTTP/1.1") :: List.map (\( n, v ) -> n ++ ": " ++ v) (( "Host", "x" ) :: headers)) ++ crlf ++ crlf


{-| A connection accepted, its effects forgotten, ready for a head.
-}
connected : Server.Config -> Sim
connected cfg =
    feed (Accepted conn (clockAt 0)) { model = serving cfg, owner = Exchange a, machine = Nothing, effects = [], performed = [], now = 0 }
        |> (\s -> { s | effects = [] })


arriveWith : Server.Config -> String -> String -> List ( String, String ) -> Sim
arriveWith cfg method target headers =
    connected cfg |> feed (HeadArrived conn (headText method target headers) (clockAt 0))


arrive : String -> String -> List ( String, String ) -> Sim
arrive =
    arriveWith config


withBody : String -> List ( String, String )
withBody body =
    [ ( "Content-Length", String.fromInt (String.length body) ) ]


{-| A server just started, following its janitor.
-}
booting : Server.Config -> Sim
booting cfg =
    let
        ( model, effects ) =
            Server.init cfg
    in
    List.foldl applyEffect { model = model, owner = Janitor, machine = Nothing, effects = effects, performed = [], now = 0 } effects


feed : Msg -> Sim -> Sim
feed msg sim =
    let
        ( model, effects ) =
            Server.step msg sim.model
    in
    List.foldl applyEffect { sim | model = model, effects = sim.effects ++ effects } effects


tick : Int -> Sim -> Sim
tick mono sim =
    feed (Woke (clockAt mono)) { sim | now = mono }


progress : Int -> Bool -> Int -> Sim -> Sim
progress bytes running mono sim =
    feed (Progressed a bytes running (clockAt mono)) sim


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
    List.foldl (\r s -> feed (Reported s.owner (Tx.encodeReport r) (clockAt s.now)) s) { sim | machine = Just machine, performed = started } reports


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


{-| Every response Elm wrote: status and body.
-}
answers : Sim -> List ( Int, String )
answers sim =
    List.filterMap
        (\effect ->
            case effect of
                Send _ text ->
                    let
                        status =
                            String.split " " text |> List.drop 1 |> List.head |> Maybe.andThen String.toInt |> Maybe.withDefault 0

                        body =
                            case String.indexes (crlf ++ crlf) text of
                                i :: _ ->
                                    String.dropLeft (i + 4) text

                                [] ->
                                    ""
                    in
                    Just ( status, body )

                _ ->
                    Nothing
        )
        sim.effects


{-| The head of every response Elm wrote.
-}
heads : Sim -> List String
heads sim =
    List.filterMap
        (\effect ->
            case effect of
                Send _ text ->
                    String.indexes (crlf ++ crlf) text |> List.head |> Maybe.map (\i -> String.left i text)

                _ ->
                    Nothing
        )
        sim.effects


one : (( Int, String ) -> Expectation) -> Sim -> Expectation
one check sim =
    case answers sim of
        [ w ] ->
            check w

        out ->
            Expect.fail ("expected exactly one response, got " ++ Debug.toString (List.map Tuple.first out))


statusIs : Int -> ( Int, String ) -> Expectation
statusIs code ( status, body ) =
    Expect.equal code status |> Expect.onFail body


{-| What became of the connection after each response.
-}
fates : Sim -> List String
fates sim =
    List.filterMap
        (\effect ->
            case effect of
                AwaitHead _ _ ->
                    Just "await"

                CloseConn _ ->
                    Just "close"

                Dissipate _ _ ->
                    Just "dissipate"

                _ ->
                    Nothing
        )
        sim.effects


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
                Send _ _ ->
                    Just "send"

                Abort _ ->
                    Just "abort"

                Extend _ _ ->
                    Just "extend"

                Begin _ _ ->
                    Just "begin"

                AwaitHead _ _ ->
                    Just "await"

                CloseConn _ ->
                    Just "close"

                Dissipate _ _ ->
                    Just "dissipate"

                Listen ->
                    Just "listen"

                Halt _ ->
                    Just "halt"

                Report _ _ ->
                    Nothing

                Note _ _ _ _ ->
                    Nothing

                Alarm _ ->
                    Nothing
        )
        sim.effects


alarms : Sim -> List (Maybe Int)
alarms sim =
    List.filterMap
        (\effect ->
            case effect of
                Alarm inMs ->
                    Just inMs

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
commitOps fill final dir =
    [ "open /d/.tmp/c.0.part", fill, "fsync", "link " ++ final, "unlink /d/.tmp/c.0.part", "fsync-dir " ++ dir, "close" ]


closedEvent : String -> Int -> Encode.Value
closedEvent how discarded =
    Encode.object
        [ ( "event", Encode.string "closed" )
        , ( "conn", Encode.string "c" )
        , ( "how", Encode.string how )
        , ( "discarded", Encode.int discarded )
        , ( "mono", Encode.float 12.75 )
        , ( "wall", Encode.int 784111777000 )
        ]



-- TESTS


suite : Test
suite =
    describe "Server.step"
        [ describe "buffered ingest"
            [ test "the happy path: read the body, pause, then one commit transaction, 201 last, the connection kept" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (host payload)
                        |> Expect.all
                            [ names >> Expect.equal ("read-body" :: commitOps "write" "/d/file_42.final" "/d")
                            , one (Expect.equal ( 201, "{\"itemId\":42,\"bytes\":17,\"sha256\":\"abc123\"}" ))
                            , fates >> Expect.equal [ "await" ]
                            , settled
                            ]
            , test "poison disk: EACCES on the lease is a 500 naming the host error, without server paths" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (failing isOpen "EACCES" "open" payload)
                        |> Expect.all
                            [ one
                                (\( status, body ) ->
                                    Expect.equal ( 500, True, False )
                                        ( status
                                        , String.contains "\"hostError\":{\"code\":\"EACCES\",\"syscall\":\"open\"}" body
                                        , String.contains "/secret" body
                                        )
                                )
                            , names >> Expect.equal [ "read-body", "open /d/.tmp/c.0.part" ]
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
            , test "a link that finds the final file is 409, and the rollback removes only our part" <|
                \_ ->
                    arrive "POST" "/ingest/42" (withBody payload)
                        |> answerAll (failing isLink "EEXIST" "link" payload)
                        |> Expect.all
                            [ one (statusIs 409)
                            , names >> List.drop 4 >> Expect.equal [ "link /d/file_42.final", "unlink /d/.tmp/c.0.part", "close" ]
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
                            , one (Expect.equal ( 201, "{\"blobId\":9,\"bytes\":10485760,\"sha256\":\"f00d\"}" ))
                            , settled
                            ]
            , test "the 100 Continue invitation rides on the body step, which comes after the lease" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ), ( "Expect", "100-continue" ) ]
                        |> answer (host "")
                        |> answer (host "")
                        |> .performed
                        |> List.take 2
                        |> List.map
                            (\op ->
                                case op of
                                    Io.PipeBody pipe ->
                                        Maybe.withDefault "pipe without invitation" pipe.invite

                                    _ ->
                                        opName op
                            )
                        |> Expect.equal [ "open /d/.tmp/c.0.part", "HTTP/1.1 100 Continue" ++ crlf ++ crlf ]
            , test "a declared length over the limit is refused from the head: no transaction, the connection dissipated" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10737418240" ) ]
                        |> Expect.all
                            [ one (statusIs 413)
                            , kinds >> Expect.equal [ "send", "dissipate" ]
                            ]
            , test "the client leaves mid-body: abort, the pipe is severed, unlink then close, no answer" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> feed (ConnClosed conn Server.PeerLeft (clockAt 0))
                        |> answerAll (host "")
                        |> Expect.all
                            [ names >> Expect.equal [ "open /d/.tmp/c.0.part", "pipe-body", "unlink /d/.tmp/c.0.part", "close" ]
                            , answers >> Expect.equal []
                            , kinds >> Expect.equal [ "begin", "abort" ]
                            , \sim -> Expect.equal 1 sim.model.ledger.abandoned
                            , settled
                            ]
            , test "a slowloris body: the 408, then the abort that frees the socket, then the dissipation, then the rollback" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> progress 0 True 0
                        |> progress 300 True 500
                        |> tick 1100
                        |> answerAll (host "")
                        |> Expect.all
                            [ kinds >> Expect.equal [ "begin", "send", "abort", "dissipate" ]
                            , one (statusIs 408)
                            , heads >> List.concatMap (String.split crlf) >> List.member "connection: close" >> Expect.equal True
                            , names >> Expect.equal [ "open /d/.tmp/c.0.part", "pipe-body", "unlink /d/.tmp/c.0.part", "close" ]
                            , \sim -> Expect.equal 1 sim.model.ledger.tooSlow
                            , settled
                            ]
            , test "a body that keeps pace is left alone and the meter moves its bar" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> progress 0 True 0
                        |> progress 9000 True 300
                        |> tick 1100
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
                        |> progress 0 True 0
                        |> progress 3000 True 10
                        |> tick 1000
                        |> progress 6000 True 110
                        |> tick 1100
                        |> kinds
                        |> Expect.equal [ "begin" ]
            , test "the window opens when the bridge says the step started, not when Elm last heard the clock" <|
                \_ ->
                    -- Elm's clock says 500; the bridge started the body step at 900.
                    -- The first verdict is due at 900 + 1000 + 100, not before.
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> tick 500
                        |> answer (host "")
                        |> progress 0 True 900
                        |> tick 1700
                        |> kinds
                        |> Expect.equal [ "begin" ]
            , test "a slow disk after the body arrived is never blamed on the client: stale rate checks do nothing" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "100" ) ]
                        |> answer (host "")
                        |> progress 0 True 0
                        |> progress 100 False 50
                        |> tick 1100
                        |> kinds
                        |> Expect.equal [ "begin" ]
            , test "a client that leaves after the commit point: the commit rolls forward, nobody is answered" <|
                \_ ->
                    arrive "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]
                        |> answer (host "")
                        |> answer (host "")
                        |> answer (host "")
                        |> answer (host "")
                        |> feed (ConnClosed conn Server.PeerLeft (clockAt 0))
                        |> answerAll (host "")
                        |> Expect.all
                            [ names >> Expect.equal (commitOps "pipe-body" "/d/blobs/9" "/d/blobs")
                            , answers >> Expect.equal []
                            , settled
                            ]
            ]
        , describe "downloads"
            [ test "open, stat, pause; then the stream writes its own head and bytes, and the connection is kept after it" <|
                \_ ->
                    arrive "GET" "/blobs/1" []
                        |> answerAll (host "")
                        |> Expect.all
                            [ names >> Expect.equal [ "open /d/blobs/1", "stat", "stream", "close" ]
                            , answers >> Expect.equal []
                            , fates >> Expect.equal [ "await" ]
                            , \sim -> Expect.equal 1 sim.model.ledger.responded
                            , settled
                            ]
            , test "the stream's head is Elm's, exact length and date, and promises to close when the client asked to" <|
                \_ ->
                    arrive "GET" "/blobs/1" [ ( "Connection", "close" ) ]
                        |> answerAll (host "")
                        |> .performed
                        |> List.filterMap
                            (\op ->
                                case op of
                                    Io.Stream stream ->
                                        Just stream.head

                                    _ ->
                                        Nothing
                            )
                        |> Expect.equal
                            [ String.join crlf
                                [ "HTTP/1.1 200 OK"
                                , "content-type: application/octet-stream"
                                , "content-length: 10485760"
                                , "date: Sun, 06 Nov 1994 08:49:37 GMT"
                                , "connection: close"
                                , ""
                                , ""
                                ]
                            ]
            , test "a slow reader is aborted by the delivery meter, its fd still closes, and so does its connection" <|
                \_ ->
                    arrive "GET" "/blobs/1" []
                        |> answer (host "")
                        |> answer (host "")
                        |> progress 0 True 0
                        |> tick 1100
                        |> answerAll (host "")
                        |> Expect.all
                            [ kinds >> Expect.equal [ "begin", "extend", "abort", "close" ]
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
                            [ one (\( status, body ) -> Expect.equal ( 500, True ) ( status, String.contains "\"fatal\":true" body ))
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
            [ test "the deadline answers 504 and aborts; the connection is kept; the severed call answers nobody" <|
                \_ ->
                    arrive "GET" "/delayed/42" []
                        |> tick 2000
                        |> answerAll (host "")
                        |> Expect.all [ one (statusIs 504), kinds >> Expect.equal [ "begin", "send", "abort", "await" ], settled ]
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
            [ test "a POST to an unknown path gets 404 and its connection is dissipated, not kept" <|
                \_ ->
                    arrive "POST" "/nope" (withBody "xxxx")
                        |> Expect.all [ one (statusIs 404), fates >> Expect.equal [ "dissipate" ] ]
            , test "a slow JSON body is answered 408 by the meter" <|
                \_ ->
                    arrive "POST" "/echo" (withBody payload)
                        |> progress 0 True 0
                        |> tick 1100
                        |> answerAll (host payload)
                        |> Expect.all [ one (statusIs 408), settled ]
            , test "echo reflects the exact text" <|
                \_ ->
                    let
                        sent =
                            "{\"n\":12345678901234567890,\"b\":1,\"b\":2}"
                    in
                    arrive "POST" "/echo" (withBody sent) |> answerAll (host sent) |> one (Expect.equal ( 200, sent ))
            , test "an unknown method is 501 and 405 carries Allow" <|
                \_ ->
                    Expect.equal ( [ 501 ], [ 405 ] )
                        ( arrive "PURGE" "/health" [] |> answers |> List.map Tuple.first
                        , arrive "PUT" "/health" [] |> answers |> List.map Tuple.first
                        )
            ]
        , describe "connections"
            [ test "a persistent connection awaits its next head under idleMs; Connection: close closes it" <|
                \_ ->
                    Expect.equal
                        ( [ "await" ], [ "close" ], Just (Server.AwaitingHead { deadline = 5000, idle = True }) )
                        ( arrive "GET" "/health" [] |> fates
                        , arrive "GET" "/health" [ ( "Connection", "close" ) ] |> fates
                        , arrive "GET" "/health" [] |> .model |> .conns |> Dict.get "c" |> Maybe.map .phase
                        )
            , test "the next request on the connection is read only after the response, and is named c.1" <|
                \_ ->
                    arrive "GET" "/health" []
                        |> feed (HeadArrived conn (headText "GET" "/items/123" []) (clockAt 10))
                        |> .effects
                        |> List.filterMap
                            (\effect ->
                                case effect of
                                    Note id _ method target ->
                                        Just ( Request.idToString id, method, target )

                                    _ ->
                                        Nothing
                            )
                        |> Expect.equal [ ( "c.0", "GET", "/health" ), ( "c.1", "GET", "/items/123" ) ]
            , test "a first head that does not arrive within headDeadlineMs is answered 408 and the connection dissipated" <|
                \_ ->
                    connected config
                        |> tick 9999
                        |> tick 10000
                        |> Expect.all [ one (statusIs 408), fates >> Expect.equal [ "dissipate" ], \sim -> Expect.equal 1 sim.model.ledger.timedOut ]
            , test "a head that crosses Elm's decision to close is dropped: no second answer, no anomaly" <|
                \_ ->
                    connected config
                        |> tick 10000
                        |> feed (HeadArrived conn (headText "GET" "/health" []) (clockAt 10001))
                        |> Expect.all [ answers >> List.map Tuple.first >> Expect.equal [ 408 ], faults >> Expect.equal [] ]
            , test "an idle connection past idleMs is closed without a word" <|
                \_ ->
                    arrive "GET" "/health" []
                        |> tick 5000
                        |> Expect.all [ answers >> List.length >> Expect.equal 1, fates >> Expect.equal [ "await", "close" ] ]
            , test "a head the parser refuses is answered from the parser's verdict, and the connection dissipated" <|
                \_ ->
                    arrive "POST" "/echo" [ ( "Content-Length", "4" ), ( "Transfer-Encoding", "chunked" ) ]
                        |> Expect.all [ one (statusIs 400), fates >> Expect.equal [ "dissipate" ] ]
            , test "a head that overflows its limit is 431" <|
                \_ ->
                    connected config
                        |> feed (HeadOverflow conn (clockAt 0))
                        |> Expect.all [ one (statusIs 431), fates >> Expect.equal [ "dissipate" ] ]
            , test "the peer's FIN while a head is awaited closes the connection" <|
                \_ ->
                    connected config
                        |> feed (PeerEnded conn (clockAt 0))
                        |> Expect.all [ answers >> Expect.equal [], fates >> Expect.equal [ "close" ] ]
            , test "the peer's FIN mid-exchange: the client has left, its work is aborted, and its connection closed" <|
                \_ ->
                    arrive "GET" "/delayed/42" []
                        |> feed (PeerEnded conn (clockAt 0))
                        |> answerAll (host "")
                        |> Expect.all
                            [ answers >> Expect.equal []
                            , kinds >> Expect.equal [ "begin", "abort", "close" ]
                            , \sim -> Expect.equal 1 sim.model.ledger.abandoned
                            , settled
                            ]
            , test "a full pool closes at once, and the server counts it" <|
                \_ ->
                    arriveWith { config | linger = { deadlineMs = 500, budgetBytes = 131072, maxSockets = 0 } } "POST" "/nope" (withBody "xxxx")
                        |> Expect.all [ fates >> Expect.equal [ "close" ], \sim -> Expect.equal 1 sim.model.ledger.lingered.poolFull ]
            , test "a dissipated connection's slot comes back when its fd closes, and how it ended is counted" <|
                \_ ->
                    arrive "POST" "/nope" (withBody "xxxx")
                        |> feed (ConnClosed conn (Server.Dissipated Kernel.BudgetSpent 196608) (clockAt 600))
                        |> Expect.all
                            [ \sim -> Expect.equal 0 (Kernel.held sim.model.pool)
                            , \sim -> Expect.equal ( 1, 196608 ) ( sim.model.ledger.lingered.budgetSpent, sim.model.ledger.lingered.bytesDiscarded )
                            , \sim -> Expect.equal 0 (Dict.size sim.model.conns)
                            ]
            , test "the alarm: every tickMs while a head is awaited, at the next sweep when nothing waits, none once stopped" <|
                \_ ->
                    connected config
                        |> feed (PeerEnded conn (clockAt 50))
                        |> feed (ConnClosed conn Server.AsAsked (clockAt 60))
                        |> tick 100
                        |> feed (ServerClosed (clockAt 200))
                        |> alarms
                        |> Expect.equal [ Just (1000000000 - 100), Nothing ]
            , test "events decode from the bridge's JSON; one that does not is an anomaly, never a guess" <|
                \_ ->
                    Expect.equal
                        ( [ Just (ConnClosed conn (Server.Dissipated Kernel.BudgetSpent 196608) { mono = 12, wall = 784111777000 }), Nothing ]
                        , 1
                        )
                        ( List.map (Decode.decodeValue Server.eventDecoder >> Result.toMaybe) [ closedEvent "budget" 196608, closedEvent "evaporated" 0 ]
                        , connected config |> feed (Garbled "a closed event without a conn") |> faults |> List.length
                        )
            , test "negative linger bounds refuse to start" <|
                \_ ->
                    Server.init { config | linger = { deadlineMs = 500, budgetBytes = -1, maxSockets = 64 } }
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
                            , alarms >> Expect.equal []
                            ]
            , test "a server closed while it boots never opens" <|
                \_ ->
                    booting config
                        |> feed (ServerClosed (clockAt 0))
                        |> answerAll (janitorHost 0 [] (always Nothing))
                        |> kinds
                        |> Expect.equal [ "begin" ]
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
                            , \sim -> Expect.equal config.sweepEveryMs sim.model.nextSweepAt
                            ]
            , test "staleness is judged by the host's wall clock at the listing, never by Elm's" <|
                \_ ->
                    booting config
                        |> tick 99999999999
                        |> answerAll (janitorHost 10000000 [ File "young.part" 9000000 ] (always Nothing))
                        |> doomed
                        |> Expect.equal []
            , test "a part held by a live upload is never taken, even when its mtime says stale" <|
                \_ ->
                    booting config
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> feed (Accepted conn (clockAt 0))
                        |> feed (HeadArrived conn (headText "PUT" "/blobs/9" [ ( "Content-Length", "10485760" ) ]) (clockAt 0))
                        |> answerAll (janitorHost 99999999 [ File "c.0.part" 0, File "dead.part" 0 ] (always Nothing))
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
            , test "the next sweep starts on the first alarm at or after nextSweepAt, and only between sweeps" <|
                \_ ->
                    booting config
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> answer (janitorHost 0 [] (always Nothing))
                        |> tick 10
                        |> answerAll (failing (always True) "EIO" "scandir" "")
                        |> tick (config.sweepEveryMs - 1)
                        |> tick (config.sweepEveryMs + 10)
                        |> Expect.all
                            [ kinds >> Expect.equal [ "begin", "listen", "begin", "begin" ]
                            , faults >> List.length >> Expect.equal 1
                            ]
            ]
        , describe "layout"
            [ test "partFromName inverts part for a request id, and refuses names part could not have made" <|
                \_ ->
                    let
                        layout =
                            Layout.fromDataDir "/d"

                        id =
                            "0f8fad5b-d9cb-469f-a165-70867728950e.3"
                    in
                    Expect.equal
                        ( Just (Layout.part layout (Request.idFromString id)), [ Nothing, Nothing, Nothing, Nothing ] )
                        ( Layout.partFromName layout (id ++ ".part")
                        , List.map (Layout.partFromName layout) [ id ++ ".tmp", ".part", "notes", "blob_9.tmp" ]
                        )
            , test "a part is named after its transaction, never after the item it will become" <|
                \_ ->
                    Layout.partPath (Layout.part (Layout.fromDataDir "/d") (Request.idWithin conn 7))
                        |> Expect.equal "/d/.tmp/c.7.part"
            ]
        ]
