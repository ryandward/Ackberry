port module Main exposing (main)

{-| Port wiring only. Every decision is made in `Server.step`.

One port each way. Inbound, every event the bridge reports, decoded by
`Server.eventDecoder`; an event that does not decode becomes `Garbled`,
never a silent drop. Outbound, each update sends at most one message: the
ordered array of instructions that update produced. `Cmd.batch` reverses
commands on a single port and races commands on different ports, so
nothing that needs an order is ever split across two `Cmd`s.

There are no Elm timers. The clock is the bridge's, read at each event,
and the only alarm is the one `Server` asks for.

-}

import Http.Request as Request
import Io
import Json.Decode as Decode
import Json.Encode as Encode
import Kernel
import Plan
import Server
import Tx
import Verdict


port fromBridge : (Decode.Value -> msg) -> Sub msg


port toBridge : List Encode.Value -> Cmd msg


main : Program Server.Config Server.Model Server.Msg
main =
    Platform.worker
        { init =
            \config ->
                let
                    ( model, effects ) =
                        Server.init config
                in
                ( model, send effects )
        , update =
            \msg model ->
                let
                    ( next, effects ) =
                        Server.step msg model
                in
                ( next, send effects )
        , subscriptions =
            \_ ->
                fromBridge
                    (\value ->
                        case Decode.decodeValue Server.eventDecoder value of
                            Ok msg ->
                                msg

                            Err error ->
                                Server.Garbled (Decode.errorToString error)
                    )
        }


{-| An update's effects as one message, or none. The init effects go out
the same way: an outgoing port delivers nothing before the bridge has
subscribed, because its manager starts with a zero-delay sleep.
-}
send : List Server.Effect -> Cmd msg
send effects =
    case effects of
        [] ->
            Cmd.none

        _ ->
            toBridge (List.map instruction effects)


instruction : Server.Effect -> Encode.Value
instruction effect =
    let
        doing name fields =
            Encode.object (( "do", Encode.string name ) :: fields)

        connField conn =
            ( "conn", Encode.string (Request.connIdToString conn) )
    in
    case effect of
        Server.Send conn text ->
            doing "send" [ connField conn, ( "text", Encode.string text ) ]

        Server.AwaitHead conn maxBytes ->
            doing "awaitHead" [ connField conn, ( "maxBytes", Encode.int maxBytes ) ]

        Server.CloseConn conn ->
            doing "close" [ connField conn ]

        Server.Dissipate conn profile ->
            doing "dissipate"
                [ connField conn
                , ( "deadlineMs", Encode.int profile.deadlineMs )
                , ( "budgetBytes", Encode.int profile.budgetBytes )
                , ( "whenSpent"
                  , Encode.string <|
                        case profile.whenSpent of
                            Kernel.Cut ->
                                "cut"

                            Kernel.Hold ->
                                "hold"
                  )
                ]

        Server.Begin owner program ->
            Tx.encodeBegin (Server.ownerKey owner) (Plan.steps program) (Plan.continuation program)

        Server.Extend owner program ->
            Tx.encodeExtend (Server.ownerKey owner) (Plan.steps program) (Plan.continuation program)

        Server.Abort id ->
            Tx.encodeAbort (Server.ownerKey (Server.Exchange id))

        Server.Report owner fault ->
            doing "log" [ ( "fault", encodeFault owner fault ) ]

        Server.Note id conn method target ->
            doing "note"
                [ ( "request", Encode.string (Request.idToString id) )
                , connField conn
                , ( "method", Encode.string method )
                , ( "target", Encode.string target )
                ]

        Server.Alarm inMs ->
            doing "alarm" [ ( "inMs", Maybe.withDefault Encode.null (Maybe.map Encode.int inMs) ) ]

        Server.Listen ->
            doing "listen" []

        Server.Halt reason ->
            doing "halt" [ ( "reason", Encode.string reason ) ]


encodeFault : Server.Owner -> Verdict.Fault -> Encode.Value
encodeFault owner fault =
    Encode.object <|
        ( "tx", Encode.string (Server.ownerKey owner) )
            :: (case fault of
                    Verdict.Unclassified context error ->
                        [ ( "event", Encode.string "unclassified" )
                        , ( "context", Encode.string context )
                        , ( "error", Io.encodeHostError error )
                        ]

                    Verdict.Orphaned path error ->
                        [ ( "event", Encode.string "orphan" )
                        , ( "path", Encode.string path )
                        , ( "error", Io.encodeHostError error )
                        ]

                    Verdict.Anomaly detail ->
                        [ ( "event", Encode.string "anomaly" )
                        , ( "detail", Encode.string detail )
                        ]
               )
