port module Main exposing (main)

{-| Port wiring only. Every decision is made in `Server.step`.

One outbound port. Each update sends at most one message on it: the ordered
array of instructions that update produced. `Cmd.batch` reverses commands on
a single port and races commands on different ports, so nothing that needs an
order is ever split across two `Cmd`s.

Inbound ports carry ids as typed strings next to an untyped payload, so Elm
never holds a request or a report it cannot address.

-}

import Dict
import Http.Request as Request
import Http.Response as Response
import Io
import Json.Decode as Decode
import Json.Encode as Encode
import Plan
import Server
import Time
import Tx
import Verdict


port incomingRequest : (( String, Int, Decode.Value ) -> msg) -> Sub msg


port clientDisconnected : (String -> msg) -> Sub msg


port txReported : (( String, Decode.Value ) -> msg) -> Sub msg


port txProgress : ({ request : String, bytes : Int, running : Bool, at : Int } -> msg) -> Sub msg


port serverClosed : (() -> msg) -> Sub msg


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
            \model ->
                Sub.batch
                    [ incomingRequest (\( id, at, wire ) -> Server.Received (Request.idFromString id) at wire)
                    , clientDisconnected (Request.idFromString >> Server.ClientGone)
                    , txReported (\( tx, value ) -> Server.Reported (Server.ownerFromKey tx) value)
                    , txProgress (\p -> Server.Progressed (Request.idFromString p.request) p.bytes p.running p.at)
                    , serverClosed (\() -> Server.Closed)

                    -- The clocks run only while something needs them.
                    , if Dict.isEmpty model.jobs then
                        Sub.none

                      else
                        Time.every 100 (Time.posixToMillis >> Server.Tick)
                    , case Server.sweepEvery model of
                        Just ms ->
                            Time.every (toFloat ms) (\_ -> Server.SweepDue)

                        Nothing ->
                            Sub.none
                    ]
        }


{-| An update's effects as one message, or none. The init effects go out
the same way: an outgoing port delivers nothing before the bridge has
subscribed, because its manager starts with a zero-delay sleep.
-}
send : List Server.Effect -> Cmd msg
send effects =
    case List.filterMap instruction effects of
        [] ->
            Cmd.none

        instructions ->
            toBridge instructions


instruction : Server.Effect -> Maybe Encode.Value
instruction effect =
    case effect of
        Server.Respond id response connection ->
            let
                wire =
                    Response.toWire connection response
            in
            Just
                (Encode.object
                    [ ( "do", Encode.string "respond" )
                    , ( "request", Encode.string (Request.idToString id) )
                    , ( "status", Encode.int wire.status )
                    , ( "headers", Encode.list Encode.string wire.headers )
                    , ( "body", Encode.string wire.body )
                    ]
                )

        Server.Begin owner program ->
            Just (Tx.encodeBegin (Server.ownerKey owner) (Plan.steps program) (Plan.continuation program))

        Server.Extend owner program ->
            Just (Tx.encodeExtend (Server.ownerKey owner) (Plan.steps program) (Plan.continuation program))

        Server.Abort id ->
            Just (Tx.encodeAbort (Server.ownerKey (Server.Exchange id)))

        Server.Report owner fault ->
            Just
                (Encode.object
                    [ ( "do", Encode.string "log" )
                    , ( "fault", encodeFault owner fault )
                    ]
                )

        Server.Listen ->
            Just (Encode.object [ ( "do", Encode.string "listen" ) ])

        Server.Halt reason ->
            Just (Encode.object [ ( "do", Encode.string "halt" ), ( "reason", Encode.string reason ) ])


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
