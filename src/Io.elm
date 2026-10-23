module Io exposing
    ( Failure(..)
    , Handle
    , HostError
    , Op(..)
    , OpenMode(..)
    , classify
    , encodeHostError
    , encodeOp
    , errorCode
    , handle
    , handleToString
    , hostErrorDecoder
    , interruptible
    , outcomeDecoder
    , publicHostError
    , revokedError
    )

{-| The host primitives a transaction can contain, and the parser for what
comes back.

An `Op` is one primitive. It does nothing by itself: it only runs as a step of
a transaction (see `Tx`), which is where ordering, compensation and
preemption are defined.

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- HANDLES


{-| The name of a file descriptor the bridge holds for a transaction. Elm
mints the name before the fd exists, so a whole transaction can be written
down up front; the `Open` step is what binds the name to a real fd, and the
bridge closes anything a transaction opened and forgot.
-}
type Handle
    = Handle String


handle : String -> String -> Handle
handle owner name =
    Handle (owner ++ "/" ++ name)


handleToString : Handle -> String
handleToString (Handle name) =
    name



-- OPERATIONS


type OpenMode
    = CreateExclusive
    | ReadOnly


type Op
    = Open { handle : Handle, mode : OpenMode, path : String }
    | Write Handle String
    | Sync Handle
    | Close Handle
    | Stat Handle
    | Link { from : String, to : String }
    | Unlink String
    | SyncDir String
    | HttpGet { url : String, maxBytes : Int }
    | ReadBody { maxBytes : Int, sendContinue : Bool }
    | PipeBody { handle : Handle, maxBytes : Int, sendContinue : Bool }
    | Stream { handle : Handle, length : Int, status : Int, headers : List String }


{-| Which primitives observe an abort while in flight. Sockets and streams
can be severed in userland; a filesystem syscall already handed to the
libuv threadpool cannot be interrupted by anything short of SIGKILL, so an
abort that lands during one waits for it to finish.
-}
interruptible : Op -> Bool
interruptible op =
    case op of
        ReadBody _ ->
            True

        PipeBody _ ->
            True

        Stream _ ->
            True

        HttpGet _ ->
            True

        Open _ ->
            False

        Write _ _ ->
            False

        Sync _ ->
            False

        Close _ ->
            False

        Stat _ ->
            False

        Link _ ->
            False

        Unlink _ ->
            False

        SyncDir _ ->
            False


encodeOp : Op -> Encode.Value
encodeOp op =
    let
        kind name =
            ( "kind", Encode.string name )

        handleField h =
            ( "handle", Encode.string (handleToString h) )

        path p =
            ( "path", Encode.string p )
    in
    Encode.object <|
        case op of
            Open open ->
                [ kind "open"
                , handleField open.handle
                , path open.path
                , ( "flags"
                  , Encode.list Encode.string
                        (case open.mode of
                            CreateExclusive ->
                                [ "O_WRONLY", "O_CREAT", "O_EXCL" ]

                            -- O_NONBLOCK: a FIFO planted where a file should
                            -- be would otherwise park a threadpool thread in
                            -- open(2) forever, beyond the reach of any abort.
                            ReadOnly ->
                                [ "O_RDONLY", "O_NONBLOCK" ]
                        )
                  )
                ]

            Write h text ->
                [ kind "write", handleField h, ( "text", Encode.string text ) ]

            Sync h ->
                [ kind "sync", handleField h ]

            Close h ->
                [ kind "close", handleField h ]

            Stat h ->
                [ kind "stat", handleField h ]

            Link link ->
                [ kind "link", ( "from", Encode.string link.from ), ( "to", Encode.string link.to ) ]

            Unlink p ->
                [ kind "unlink", path p ]

            SyncDir p ->
                [ kind "syncDir", path p ]

            HttpGet get ->
                [ kind "httpGet", ( "url", Encode.string get.url ), ( "maxBytes", Encode.int get.maxBytes ) ]

            ReadBody read ->
                [ kind "readBody", ( "maxBytes", Encode.int read.maxBytes ), ( "sendContinue", Encode.bool read.sendContinue ) ]

            PipeBody pipe ->
                [ kind "pipeBody"
                , handleField pipe.handle
                , ( "maxBytes", Encode.int pipe.maxBytes )
                , ( "sendContinue", Encode.bool pipe.sendContinue )
                ]

            Stream stream ->
                [ kind "stream"
                , handleField stream.handle
                , ( "length", Encode.int stream.length )
                , ( "status", Encode.int stream.status )
                , ( "headers", Encode.list Encode.string stream.headers )
                ]



-- HOST ERRORS


{-| A host failure exactly as the bridge copied it: no field interpreted,
the `cause` chain kept.
-}
type HostError
    = HostError
        { name : String
        , code : String
        , syscall : String
        , rawMessage : String
        , cause : Maybe HostError
        }


hostErrorDecoder : Decoder HostError
hostErrorDecoder =
    Decode.map5
        (\name code syscall rawMessage cause ->
            HostError { name = name, code = code, syscall = syscall, rawMessage = rawMessage, cause = cause }
        )
        (Decode.field "name" scalar)
        (Decode.field "code" scalar)
        (Decode.field "syscall" scalar)
        (Decode.field "rawMessage" Decode.string)
        (Decode.field "cause" (Decode.nullable (Decode.lazy (\_ -> hostErrorDecoder))))


{-| System errors carry string codes; DOMException carries a numeric one.
-}
scalar : Decoder String
scalar =
    Decode.oneOf
        [ Decode.string
        , Decode.map String.fromInt Decode.int
        , Decode.null ""
        ]


{-| One step's outcome: `{ ok: ... }` or `{ error: ... }`. A shape that is
neither is a bridge bug and becomes a host error nobody models, so it can
only ever surface as a fatal 500.
-}
outcomeDecoder : Decoder (Result HostError Decode.Value)
outcomeDecoder =
    Decode.oneOf
        [ Decode.map Ok (Decode.field "ok" Decode.value)
        , Decode.map Err (Decode.field "error" hostErrorDecoder)
        , Decode.map (Err << protocolError) Decode.value
        ]


protocolError : Decode.Value -> HostError
protocolError value =
    HostError
        { name = "ProtocolError"
        , code = ""
        , syscall = ""
        , rawMessage = "not an outcome: " ++ Encode.encode 0 value
        , cause = Nothing
        }


{-| The outcome a step gets when its transaction is aborted while it is in
flight and the primitive can observe it.
-}
revokedError : HostError
revokedError =
    HostError
        { name = "AbortError"
        , code = "ABORT_ERR"
        , syscall = ""
        , rawMessage = "This operation was aborted"
        , cause = Nothing
        }


errorCode : HostError -> String
errorCode (HostError error) =
    error.code


{-| The failure classes some call site has a model for. Everything else is
`Unrecognized`, and every call site sends that to a fatal 500.
`ACKBERRY_LIMIT` is the bridge's one invented code: a byte limit Elm set was
exceeded mid-stream.
-}
type Failure
    = NoEntry
    | Exists
    | Denied
    | NoSpace
    | Exhausted
    | DeviceError
    | Refused
    | Reset
    | Unreachable
    | Cancelled
    | TooLarge
    | Unrecognized


classify : HostError -> Failure
classify (HostError error) =
    case error.code of
        "ENOENT" ->
            NoEntry

        "EEXIST" ->
            Exists

        "EACCES" ->
            Denied

        "EPERM" ->
            Denied

        "EROFS" ->
            Denied

        "ENOSPC" ->
            NoSpace

        "EDQUOT" ->
            NoSpace

        "EMFILE" ->
            Exhausted

        "ENFILE" ->
            Exhausted

        "EIO" ->
            DeviceError

        "ECONNREFUSED" ->
            Refused

        "ECONNRESET" ->
            Reset

        "EPIPE" ->
            Reset

        "ERR_STREAM_PREMATURE_CLOSE" ->
            Reset

        "ENOTFOUND" ->
            Unreachable

        "EAI_AGAIN" ->
            Unreachable

        "EHOSTUNREACH" ->
            Unreachable

        "ENETUNREACH" ->
            Unreachable

        "ETIMEDOUT" ->
            Unreachable

        "ABORT_ERR" ->
            Cancelled

        "ACKBERRY_LIMIT" ->
            TooLarge

        _ ->
            Unrecognized


{-| Everything, for the server-side fault log.
-}
encodeHostError : HostError -> Encode.Value
encodeHostError (HostError error) =
    Encode.object
        [ ( "name", Encode.string error.name )
        , ( "code", Encode.string error.code )
        , ( "syscall", Encode.string error.syscall )
        , ( "rawMessage", Encode.string error.rawMessage )
        , ( "cause", Maybe.withDefault Encode.null (Maybe.map encodeHostError error.cause) )
        ]


{-| What a client may see. Raw messages name absolute server paths, so they
stay in the fault log.
-}
publicHostError : HostError -> Encode.Value
publicHostError (HostError error) =
    Encode.object
        [ ( "code", Encode.string error.code )
        , ( "syscall", Encode.string error.syscall )
        ]
