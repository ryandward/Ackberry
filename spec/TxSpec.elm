port module TxSpec exposing (main)

{-| The reference side of the differential test (tests/vm.js).

Receives an abstract program, builds it from the real `Tx` and `Io` types,
and answers with (1) the exact instructions production code would send the
bridge for it, and (2) the reports the reference interpreter says the bridge
must produce, normalized for comparison.

-}

import Dict
import Io exposing (HostError)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Tx


port program : (Decode.Value -> msg) -> Sub msg


port verdict : Encode.Value -> Cmd msg


main : Program () () Decode.Value
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \value _ -> ( (), verdict (answer value) )
        , subscriptions = \_ -> program identity
        }


type alias Abstract =
    { segments : List ( List Tx.Step, Tx.Continuation )
    , outcomes : Dict.Dict String (Result HostError Decode.Value)
    , abort : Maybe Tx.AbortPoint
    }


answer : Decode.Value -> Encode.Value
answer value =
    case Decode.decodeValue abstractDecoder value of
        Err error ->
            Encode.object [ ( "error", Encode.string (Decode.errorToString error) ) ]

        Ok p ->
            let
                tx =
                    "tx"

                lookup _ op =
                    Dict.get (labelOf op) p.outcomes |> Maybe.withDefault (Ok (Encode.object []))

                reports =
                    Tx.run { step = lookup, cleanup = lookup, abort = p.abort } p.segments
            in
            Encode.object
                [ ( "instructions"
                  , Encode.list identity
                        (List.indexedMap
                            (\i ( steps, continuation ) ->
                                if i == 0 then
                                    Tx.encodeBegin tx steps continuation

                                else
                                    Tx.encodeExtend tx steps continuation
                            )
                            p.segments
                        )
                  )
                , ( "reports", Encode.list normalize reports )
                ]


normalize : Tx.Report -> Encode.Value
normalize r =
    let
        outcome result =
            case result of
                Ok _ ->
                    "ok"

                Err error ->
                    "err:" ++ Io.errorCode error
    in
    Encode.object
        [ ( "status"
          , Encode.string
                (case r.status of
                    Tx.Paused ->
                        "paused"

                    Tx.Settled ->
                        "settled"
                )
          )
        , ( "committed", Encode.bool r.committed )
        , ( "aborted", Encode.bool r.aborted )
        , ( "steps", Encode.list (outcome >> Encode.string) r.steps )
        , ( "cleanups", Encode.list (\c -> Encode.string (c.kind ++ "|" ++ Maybe.withDefault "" c.path ++ "|" ++ outcome c.outcome)) r.cleanups )
        ]


labelOf : Io.Op -> String
labelOf op =
    case op of
        Io.Unlink name ->
            name

        Io.HttpGet get ->
            get.url

        _ ->
            ""


abstractDecoder : Decoder Abstract
abstractDecoder =
    Decode.map3 Abstract
        (Decode.field "segments" (Decode.list segmentDecoder))
        (Decode.field "outcomes" (Decode.dict outcomeDecoder))
        (Decode.field "abort" (Decode.nullable abortDecoder))


segmentDecoder : Decoder ( List Tx.Step, Tx.Continuation )
segmentDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "steps" (Decode.list stepDecoder))
        (Decode.field "then"
            (Decode.string
                |> Decode.map
                    (\c ->
                        if c == "pause" then
                            Tx.Pause

                        else
                            Tx.Finish
                    )
            )
        )


stepDecoder : Decoder Tx.Step
stepDecoder =
    Decode.field "commit" Decode.bool
        |> Decode.andThen
            (\commit ->
                if commit then
                    Decode.map Tx.Commit (Decode.field "op" opDecoder)

                else
                    Decode.map2 Tx.Do (Decode.field "op" opDecoder) (Decode.field "cleanups" (Decode.list cleanupDecoder))
            )


cleanupDecoder : Decoder Tx.Cleanup
cleanupDecoder =
    Decode.map2
        (\always op ->
            if always then
                Tx.Always op

            else
                Tx.OnFailure op
        )
        (Decode.field "always" Decode.bool)
        (Decode.field "op" opDecoder)


opDecoder : Decoder Io.Op
opDecoder =
    Decode.map2
        (\interruptible name ->
            if interruptible then
                Io.HttpGet { url = name, maxBytes = 1 }

            else
                Io.Unlink name
        )
        (Decode.field "interruptible" Decode.bool)
        (Decode.field "label" Decode.string)


outcomeDecoder : Decoder (Result HostError Decode.Value)
outcomeDecoder =
    Decode.nullable Decode.string
        |> Decode.map
            (\code ->
                case code of
                    Nothing ->
                        Ok (Encode.object [])

                    Just c ->
                        Decode.decodeValue Io.hostErrorDecoder
                            (Encode.object
                                [ ( "name", Encode.string "Error" )
                                , ( "code", Encode.string c )
                                , ( "syscall", Encode.string "" )
                                , ( "rawMessage", Encode.string c )
                                , ( "cause", Encode.null )
                                ]
                            )
                            |> Result.withDefault Io.revokedError
                            |> Err
            )


abortDecoder : Decoder Tx.AbortPoint
abortDecoder =
    Decode.oneOf
        [ Decode.map Tx.DuringStep (Decode.field "duringStep" Decode.int)
        , Decode.map Tx.WhilePaused (Decode.field "whilePaused" Decode.int)
        ]
