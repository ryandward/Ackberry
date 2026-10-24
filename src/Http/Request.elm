module Http.Request exposing
    ( Body(..)
    , BodyProblem(..)
    , ConnId
    , Framing(..)
    , JsonText
    , Method(..)
    , Request
    , RequestId
    , allMethods
    , bodyDecoder
    , connIdFromString
    , connIdToString
    , idFromString
    , idToString
    , idWithin
    , jsonFrom
    , jsonTextToString
    , methodToString
    , splitTarget
    )

{-| Requests as the router sees them, the ids that name requests and
connections, and the bodies a step reads.

A request is what `Http.Head` made of the head the client sent. Its body
stays on the socket, unread, until a transaction step asks for it: what the
head says about the body is all Elm knows up front, and is enough to refuse
it without reading it.

-}

import Json.Decode as Decode exposing (Decoder)


type RequestId
    = RequestId String


idFromString : String -> RequestId
idFromString =
    RequestId


{-| A TCP connection, named by the bridge with `randomUUID` when it accepts
it, so that no two connections share a name in this process or any other.
-}
type ConnId
    = ConnId String


connIdFromString : String -> ConnId
connIdFromString =
    ConnId


connIdToString : ConnId -> String
connIdToString (ConnId id) =
    id


{-| The `n`th request on a connection. A request's transaction and its
part are named after it, so the connection's uniqueness makes theirs.
-}
idWithin : ConnId -> Int -> RequestId
idWithin (ConnId conn) n =
    RequestId (conn ++ "." ++ String.fromInt n)


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


{-| Bytes the bridge read into memory because a step asked it to. It
transcodes them with a fatal UTF-8 decoder, so a body is either exact text or
an explicit reason it is not.
-}
type Body
    = Text String
    | TooLarge Int
    | NotUtf8


{-| How the head frames the body (RFC 9112 section 6.3): none, the
`Content-Length` bytes (never 0, which the head reads as none), or the
chunked coding, whose length is known only at its last chunk.
-}
type Framing
    = NoBody
    | Length Int
    | Chunked


type alias Request =
    { method : Method
    , path : String
    , query : Maybe String
    , headers : List ( String, String )
    , body : Framing
    , expectsContinue : Bool
    }


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
