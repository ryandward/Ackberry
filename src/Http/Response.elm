module Http.Response exposing
    ( Connection(..)
    , Failure(..)
    , Response
    , Wire
    , created
    , emptyOctets
    , ok
    , okJsonText
    , problem
    , status
    , streamHead
    , toWire
    )

{-| Wire response types and builders.

Success and failure are separate constructors, so a 2xx cannot carry a
problem document and an error status cannot carry a success body. The status
set is closed, and a 405 cannot be built without the list of allowed methods
that RFC 9110 requires in its Allow header. Header names and values come only
from this module, which is what keeps `res.writeHead` in the bridge from
throwing on a bad header.

-}

import Http.Request as Request exposing (JsonText, Method)
import Json.Encode as Encode


type Response
    = Success Code Payload
    | Problem Failure String (List ( String, Encode.Value ))


type Code
    = Ok200
    | Created201


type Payload
    = JsonValue Encode.Value
    | JsonVerbatim JsonText
    | NoOctets


type Failure
    = BadRequest
    | NotFound
    | MethodNotAllowed (List Method)
    | Conflict
    | RequestTimeout
    | PayloadTooLarge
    | InternalServerError
    | NotImplemented
    | BadGateway
    | ServiceUnavailable
    | GatewayTimeout
    | InsufficientStorage


{-| `CloseAfter` when the request declared a body that was never fully
read. Otherwise Node would keep the connection alive by reading and
discarding the rest of that body, at whatever speed the client chooses.
-}
type Connection
    = KeepAlive
    | CloseAfter


ok : Encode.Value -> Response
ok value =
    Success Ok200 (JsonValue value)


{-| 200 whose body is the exact JSON text the client sent.
-}
okJsonText : JsonText -> Response
okJsonText text =
    Success Ok200 (JsonVerbatim text)


created : Encode.Value -> Response
created value =
    Success Created201 (JsonValue value)


{-| A zero-length binary body. Non-empty files never come through here: the
bridge streams them from an open handle, under `streamHead`.
-}
emptyOctets : Response
emptyOctets =
    Success Ok200 NoOctets


{-| An RFC 9457 problem document. `detail` is for humans; the extension
members carry the same facts for machines.
-}
problem : Failure -> String -> List ( String, Encode.Value ) -> Response
problem =
    Problem


status : Response -> Int
status response =
    case response of
        Success code _ ->
            codeNumber code

        Problem failure _ _ ->
            failureCode failure


{-| Status and headers for a body the bridge streams from an open file.
The exact length goes in Content-Length and in the byte range the bridge
reads, so a file that grows while streaming cannot overrun the framing.
-}
streamHead : Int -> { status : Int, headers : List String }
streamHead length =
    { status = 200
    , headers =
        [ "content-type"
        , "application/octet-stream"
        , "content-length"
        , String.fromInt length
        ]
    }


codeNumber : Code -> Int
codeNumber code =
    case code of
        Ok200 ->
            200

        Created201 ->
            201


failureCode : Failure -> Int
failureCode failure =
    case failure of
        BadRequest ->
            400

        NotFound ->
            404

        MethodNotAllowed _ ->
            405

        Conflict ->
            409

        RequestTimeout ->
            408

        PayloadTooLarge ->
            413

        InternalServerError ->
            500

        NotImplemented ->
            501

        BadGateway ->
            502

        ServiceUnavailable ->
            503

        GatewayTimeout ->
            504

        InsufficientStorage ->
            507


failureTitle : Failure -> String
failureTitle failure =
    case failure of
        BadRequest ->
            "Bad Request"

        NotFound ->
            "Not Found"

        MethodNotAllowed _ ->
            "Method Not Allowed"

        Conflict ->
            "Conflict"

        RequestTimeout ->
            "Request Timeout"

        PayloadTooLarge ->
            "Content Too Large"

        InternalServerError ->
            "Internal Server Error"

        NotImplemented ->
            "Not Implemented"

        BadGateway ->
            "Bad Gateway"

        ServiceUnavailable ->
            "Service Unavailable"

        GatewayTimeout ->
            "Gateway Timeout"

        InsufficientStorage ->
            "Insufficient Storage"


{-| What the bridge feeds to `res.writeHead(status, headers).end(body)`.
`headers` is flat `[name, value, name, value, ...]`, the raw form writeHead
accepts, so the bridge never iterates or rewrites anything.
-}
type alias Wire =
    { status : Int
    , headers : List String
    , body : String
    }


toWire : Connection -> Response -> Wire
toWire connection response =
    let
        wire =
            render response
    in
    case connection of
        KeepAlive ->
            wire

        CloseAfter ->
            { wire | headers = wire.headers ++ [ "connection", "close" ] }


render : Response -> Wire
render response =
    case response of
        Success code payload ->
            { status = codeNumber code
            , headers =
                [ "content-type"
                , case payload of
                    JsonValue _ ->
                        "application/json; charset=utf-8"

                    JsonVerbatim _ ->
                        "application/json; charset=utf-8"

                    NoOctets ->
                        "application/octet-stream"
                ]
            , body =
                case payload of
                    JsonValue value ->
                        Encode.encode 0 value

                    JsonVerbatim text ->
                        Request.jsonTextToString text

                    NoOctets ->
                        ""
            }

        Problem failure detail extensions ->
            { status = failureCode failure
            , headers =
                [ "content-type", "application/problem+json; charset=utf-8" ]
                    ++ allowHeader failure
            , body =
                -- Standard members go last: Encode.object assigns keys in
                -- order, so an extension can never overwrite them.
                Encode.encode 0 <|
                    Encode.object
                        (extensions
                            ++ [ ( "type", Encode.string "about:blank" )
                               , ( "title", Encode.string (failureTitle failure) )
                               , ( "status", Encode.int (failureCode failure) )
                               , ( "detail", Encode.string detail )
                               ]
                        )
            }


allowHeader : Failure -> List String
allowHeader failure =
    case failure of
        MethodNotAllowed allowed ->
            [ "allow", String.join ", " (List.map Request.methodToString allowed) ]

        BadRequest ->
            []

        NotFound ->
            []

        Conflict ->
            []

        RequestTimeout ->
            []

        PayloadTooLarge ->
            []

        InternalServerError ->
            []

        NotImplemented ->
            []

        BadGateway ->
            []

        ServiceUnavailable ->
            []

        GatewayTimeout ->
            []

        InsufficientStorage ->
            []
