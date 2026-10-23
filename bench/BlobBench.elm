port module BlobBench exposing (main)

{-| Round-trips bytes through Elm the ways people try before giving up:
as a `List Int` (one cons cell per byte), as an `Array Int` (32-wide RRB
tree), and as an opaque base64 `String` Elm never decodes.
-}

import Array exposing (Array)


port listIn : (List Int -> msg) -> Sub msg


port listOut : List Int -> Cmd msg


port arrayIn : (Array Int -> msg) -> Sub msg


port arrayOut : Array Int -> Cmd msg


port base64In : (String -> msg) -> Sub msg


port base64Out : String -> Cmd msg


type Msg
    = GotList (List Int)
    | GotArray (Array Int)
    | GotBase64 String


main : Program () () Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update =
            \msg _ ->
                case msg of
                    GotList bytes ->
                        ( (), listOut bytes )

                    GotArray bytes ->
                        ( (), arrayOut bytes )

                    GotBase64 text ->
                        ( (), base64Out text )
        , subscriptions = \_ -> Sub.batch [ listIn GotList, arrayIn GotArray, base64In GotBase64 ]
        }
