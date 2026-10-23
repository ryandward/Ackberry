module Http.Request exposing
    ( Body(..)
    , BodyProblem(..)
    , Declared(..)
    , JsonText
    , Method(..)
    , Rejection(..)
    , Request
    , RequestId
    , allMethods
    , bodyDecoder
    , fromWire
    , idFromString
    , idToString
    , jsonFrom
    , jsonTextToString
    , methodToString
    )

{-| Wire request types and the single decoder from the bridge's JSON.

The bridge sends `[requestId, wire]` the moment the headers are parsed,
before a single body byte is read:

    { "method": "PUT"
    , "target": "/blobs/7?x=1"              -- raw req.url, never decoded
    , "headers": ["Host", "localhost", ...] -- raw req.rawHeaders, flat pairs
    }

The body stays on the socket, unread, until a transaction step asks for it.
What the headers say about it is all Elm knows up front, and is enough to
refuse it without reading it.

The request id travels outside `wire` on purpose: a payload that fails to
decode still has an address to answer.

-}

import Json.Decode as Decode exposing (Decoder)


type RequestId
    = RequestId String


idFromString : String -> RequestId
idFromString =
    RequestId


idToString : RequestId -> String
idToString (RequestId id) =
    id


{-| RFC 9110 methods plus PATCH (RFC 5789). Anything else is a 501, so there
is no catch-all constructor that could smuggle in a second spelling of GET.
-}
type Method
    = Get
    | Head
    | Post
    | Put
    | Patch
    | Delete
    | Options
    | Trace
    | Connect


allMethods : List Method
allMethods =
    [ Get, Head, Post, Put, Patch, Delete, Options, Trace, Connect ]


methodToString : Method -> String
methodToString method =
    case method of
        Get ->
            "GET"

        Head ->
            "HEAD"

        Post ->
            "POST"

        Put ->
            "PUT"

        Patch ->
            "PATCH"

        Delete ->
            "DELETE"

        Options ->
            "OPTIONS"

        Trace ->
            "TRACE"

        Connect ->
            "CONNECT"


methodFromString : String -> Maybe Method
methodFromString raw =
    List.filter (\method -> methodToString method == raw) allMethods
        |> List.head


{-| Bytes the bridge read into memory because a step asked it to. It
transcodes them with a fatal UTF-8 decoder, so a body is either exact text or
an explicit reason it is not.
-}
type Body
    = Text String
    | TooLarge Int
    | NotUtf8


{-| What the headers declare about the body. `Unsized` is chunked transfer
coding: the length is only known once it has all arrived.
-}
type Declared
    = NoBody
    | Length Int
    | Unsized


type alias Request =
    { method : Method
    , path : String
    , query : Maybe String
    , headers : List ( String, String )
    , body : Declared
    , expectsContinue : Bool
    }


type Rejection
    = Malformed Decode.Error
    | UnsupportedMethod String


fromWire : Decode.Value -> Result Rejection Request
fromWire value =
    case Decode.decodeValue wireDecoder value of
        Err error ->
            Err (Malformed error)

        Ok wire ->
            case methodFromString wire.method of
                Nothing ->
                    Err (UnsupportedMethod wire.method)

                Just method ->
                    let
                        ( path, query ) =
                            splitTarget wire.target
                    in
                    Ok
                        { method = method
                        , path = path
                        , query = query
                        , headers = wire.headers
                        , body = declared wire.headers
                        , expectsContinue =
                            List.any (\( name, field ) -> name == "expect" && String.toLower field == "100-continue") wire.headers
                        }


type alias Wire =
    { method : String
    , target : String
    , headers : List ( String, String )
    }


wireDecoder : Decoder Wire
wireDecoder =
    Decode.map3 Wire
        (Decode.field "method" Decode.string)
        (Decode.field "target" Decode.string)
        (Decode.field "headers" headersDecoder)


{-| Node's parser has already rejected conflicting or malformed framing
headers, so a Content-Length that reaches here is digits.
-}
declared : List ( String, String ) -> Declared
declared headers =
    if List.any (\( name, _ ) -> name == "transfer-encoding") headers then
        Unsized

    else
        case List.filter (\( name, _ ) -> name == "content-length") headers of
            ( _, value ) :: _ ->
                case String.toInt (String.trim value) of
                    Just 0 ->
                        NoBody

                    Just n ->
                        Length n

                    Nothing ->
                        Unsized

            [] ->
                NoBody


{-| Node's rawHeaders is `[name, value, name, value, ...]`. Names are
lowercased because HTTP field names are case-insensitive.
-}
headersDecoder : Decoder (List ( String, String ))
headersDecoder =
    Decode.list Decode.string
        |> Decode.andThen
            (\flat ->
                case pairUp flat [] of
                    Just pairs ->
                        Decode.succeed pairs

                    Nothing ->
                        Decode.fail "rawHeaders has an odd number of entries"
            )


pairUp : List String -> List ( String, String ) -> Maybe (List ( String, String ))
pairUp flat acc =
    case flat of
        [] ->
            Just (List.reverse acc)

        name :: value :: rest ->
            pairUp rest (( String.toLower name, value ) :: acc)

        [ _ ] ->
            Nothing


bodyDecoder : Decoder Body
bodyDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "text" ->
                        Decode.map Text (Decode.field "text" Decode.string)

                    "tooLarge" ->
                        Decode.map TooLarge (Decode.field "limit" Decode.int)

                    "notUtf8" ->
                        Decode.succeed NotUtf8

                    _ ->
                        Decode.fail ("unknown body tag " ++ tag)
            )


splitTarget : String -> ( String, Maybe String )
splitTarget target =
    case String.indexes "?" target of
        [] ->
            ( target, Nothing )

        i :: _ ->
            ( String.left i target, Just (String.dropLeft (i + 1) target) )


{-| Proof that a string is one complete JSON text. The original text is
kept instead of the parsed value because a JSON.parse/JSON.stringify round
trip rewrites the client's data: integers past 2^53 lose digits, duplicate
keys collapse, and integer-like keys get reordered.
-}
type JsonText
    = JsonText String


jsonTextToString : JsonText -> String
jsonTextToString (JsonText text) =
    text


type BodyProblem
    = BodyTooLarge Int
    | BodyNotUtf8
    | BodyNotJson


jsonFrom : Body -> Result BodyProblem JsonText
jsonFrom body =
    case body of
        Text text ->
            case Decode.decodeString Decode.value text of
                Ok _ ->
                    Ok (JsonText text)

                Err _ ->
                    Err BodyNotJson

        TooLarge limit ->
            Err (BodyTooLarge limit)

        NotUtf8 ->
            Err BodyNotUtf8
