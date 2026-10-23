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


port toBridge : List Encode.Value -> Cmd msg


main : Program Server.Config Server.Model Server.Msg
main =
    Platform.worker
        { init = \config -> ( Server.init config, Cmd.none )
        , update =
            \msg model ->
                let
                    ( next, effects ) =
                        Server.step msg model

                    instructions =
                        List.filterMap instruction effects
                in
                ( next
                , if List.isEmpty instructions then
                    Cmd.none

                  else
                    toBridge instructions
                )
        , subscriptions =
            \model ->
                Sub.batch
                    [ incomingRequest (\( id, at, wire ) -> Server.Received (Request.idFromString id) at wire)
                    , clientDisconnected (Request.idFromString >> Server.ClientGone)
                    , txReported (\( id, value ) -> Server.Reported (Request.idFromString id) value)
                    , txProgress (\p -> Server.Progressed (Request.idFromString p.request) p.bytes p.running p.at)

                    -- The clock runs only while some job needs it.
                    , if Dict.isEmpty model.jobs then
                        Sub.none

                      else
                        Time.every 100 (Time.posixToMillis >> Server.Tick)
                    ]
        }


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

        Server.Begin id program ->
            Just (Tx.encodeBegin id (Plan.steps program) (Plan.continuation program))

        Server.Extend id program ->
            Just (Tx.encodeExtend id (Plan.steps program) (Plan.continuation program))

        Server.Abort id ->
            Just (Tx.encodeAbort id)

        Server.Report id fault ->
            Just
                (Encode.object
                    [ ( "do", Encode.string "log" )
                    , ( "fault", encodeFault id fault )
                    ]
                )


encodeFault : Request.RequestId -> Verdict.Fault -> Encode.Value
encodeFault id fault =
    Encode.object <|
        ( "request", Encode.string (Request.idToString id) )
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
