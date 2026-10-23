module Verdict exposing
    ( Fault(..)
    , Verdict
    , diskFailure
    , fatal
    , hostProblem
    , orphanUnlessGone
    , quiet
    )

{-| Turning host outcomes into answers.

A verdict is what to tell the client if one is still waiting, and what the
server must hear about whether or not anyone is.

-}

import Http.Response as Response exposing (Response)
import Io exposing (HostError)
import Json.Encode as Encode


type alias Verdict =
    ( Maybe Response, List Fault )


{-| `Anomaly` is reserved for broken invariants and bridge protocol
violations; host behavior, however rare, is never an anomaly.
-}
type Fault
    = Unclassified String HostError
    | Orphaned String HostError
    | Anomaly String


{-| Nothing to say and nothing to report: the step was severed by an abort,
and whoever aborted it already answered the client.
-}
quiet : Verdict
quiet =
    ( Nothing, [] )


{-| A modeled host failure. The client gets the code and syscall, enough to
tell EACCES from ENOSPC, without server paths.
-}
hostProblem : Response.Failure -> String -> HostError -> Response
hostProblem failure detail error =
    Response.problem failure detail [ ( "hostError", Io.publicHostError error ) ]


{-| A host error this call site has no model for. Always a 500, never a 404,
never retried, and always logged with the raw message.
-}
fatal : String -> HostError -> Verdict
fatal context error =
    ( Just
        (Response.problem Response.InternalServerError
            ("unclassified host error while " ++ context)
            [ ( "fatal", Encode.bool True ), ( "hostError", Io.publicHostError error ) ]
        )
    , [ Unclassified context error ]
    )


{-| The table shared by every operation on the server's own storage.
-}
diskFailure : String -> HostError -> Verdict
diskFailure context error =
    case Io.classify error of
        Io.NoSpace ->
            ( Just (hostProblem Response.InsufficientStorage ("no space left while " ++ context) error), [] )

        Io.Exhausted ->
            ( Just (hostProblem Response.ServiceUnavailable ("out of file descriptors while " ++ context) error), [] )

        Io.Denied ->
            ( Just (hostProblem Response.InternalServerError ("the server lacks permission on its own storage while " ++ context) error), [] )

        Io.DeviceError ->
            ( Just (hostProblem Response.InternalServerError ("device I/O error while " ++ context) error), [] )

        Io.Cancelled ->
            quiet

        Io.NoEntry ->
            fatal context error

        Io.Exists ->
            fatal context error

        Io.Refused ->
            fatal context error

        Io.Reset ->
            fatal context error

        Io.Unreachable ->
            fatal context error

        Io.TooLarge ->
            fatal context error

        Io.Unrecognized ->
            fatal context error


{-| A temp file that could not be removed is an orphan: it blocks its id
with 409 until someone deletes it. It cannot be fixed from here, so it is
reported, never dropped. ENOENT means it is already gone, which is the goal.
-}
orphanUnlessGone : String -> HostError -> List Fault
orphanUnlessGone path error =
    case Io.classify error of
        Io.NoEntry ->
            []

        Io.Exists ->
            [ Orphaned path error ]

        Io.Denied ->
            [ Orphaned path error ]

        Io.NoSpace ->
            [ Orphaned path error ]

        Io.Exhausted ->
            [ Orphaned path error ]

        Io.DeviceError ->
            [ Orphaned path error ]

        Io.Refused ->
            [ Orphaned path error ]

        Io.Reset ->
            [ Orphaned path error ]

        Io.Unreachable ->
            [ Orphaned path error ]

        Io.Cancelled ->
            [ Orphaned path error ]

        Io.TooLarge ->
            [ Orphaned path error ]

        Io.Unrecognized ->
            [ Orphaned path error ]
