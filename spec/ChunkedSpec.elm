port module ChunkedSpec exposing (main)

{-| The reference side of the chunked differential test (tests/chunked.js):
each case is decoded by `Http.Chunked`, and the verdicts go back for the
bridge's decoder to match.
-}

import Http.Chunked as Chunked
import Json.Decode as Decode
import Json.Encode as Encode


port cases : (Decode.Value -> msg) -> Sub msg


port verdicts : Encode.Value -> Cmd msg


main : Program () () Decode.Value
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = \value _ -> ( (), verdicts (answer value) )
        , subscriptions = \_ -> cases identity
        }


type alias Case =
    { input : String
    , limits : Chunked.Limits
    }


answer : Decode.Value -> Encode.Value
answer value =
    case Decode.decodeValue (Decode.list caseDecoder) value of
        Ok list ->
            Encode.list verdict list

        Err error ->
            Encode.object [ ( "error", Encode.string (Decode.errorToString error) ) ]


caseDecoder : Decode.Decoder Case
caseDecoder =
    Decode.map3 (\input maxLine maxTrailer -> { input = input, limits = { maxLine = maxLine, maxTrailer = maxTrailer } })
        (Decode.field "input" Decode.string)
        (Decode.field "maxLine" Decode.int)
        (Decode.field "maxTrailer" Decode.int)


verdict : Case -> Encode.Value
verdict c =
    case Chunked.decode c.limits c.input of
        Chunked.Complete r ->
            Encode.object [ ( "kind", Encode.string "complete" ), ( "data", Encode.string r.data ), ( "used", Encode.int r.used ) ]

        Chunked.Incomplete ->
            Encode.object [ ( "kind", Encode.string "incomplete" ) ]

        Chunked.Malformed at ->
            Encode.object [ ( "kind", Encode.string "malformed" ), ( "at", Encode.int at ) ]
