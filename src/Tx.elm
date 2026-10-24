module Tx exposing
    ( AbortPoint(..)
    , Cleanup(..)
    , CleanupResult
    , Continuation(..)
    , Machine
    , Report
    , Script
    , Status(..)
    , Step(..)
    , append
    , boot
    , encodeAbort
    , encodeBegin
    , encodeExtend
    , encodeReport
    , inFlight
    , isSettled
    , land
    , preempt
    , reportDecoder
    , run
    , signalled
    )

{-| The transaction pipe.

`Cmd.batch` is an unordered bag: one port delivers a batch in reverse, two
ports race each other. So causality is not left to the runtime. It is data:

    begin
        [ Do (Open lease part) [ Always (Close lease), OnFailure (Unlink part) ]
        , Do (PipeBody lease) []
        , Do (Sync lease) []
        , Commit (Link part final)
        , Do (Unlink part) []
        , Do (SyncDir finalDir) []
        ]
        Finish

Laws, which the bridge's interpreter must obey and `run` below defines:

1.  Steps run one at a time, in list order.
2.  A step's cleanups are registered only if the step succeeds. A failed
    `Open` registers nothing, so a rollback can never delete a file this
    transaction did not create.
3.  The first failure before the commit stops the transaction. So does an
    abort: an interruptible step in flight sees the abort signal, an
    uninterruptible one is waited for, and no further step starts.
4.  After a successful `Commit`, `OnFailure` cleanups are never run. Steps
    after it run to the end whatever happens; their failures are recorded,
    not unwound, and an abort no longer stops them.
5.  The transaction ends by running the due cleanups (all of them if it
    never committed, only `Always` ones if it did) newest first. Cleanups
    never see an abort.
6.  Every transaction settles exactly once, with one report.
7.  `Pause` hands control back to Elm with the registered cleanups still
    armed. `extend` resumes it. `abort` ends it: unwound if it never
    committed, finished with its `Always` cleanups if it did. A paused
    transaction can never be left waiting for an `extend` nobody will send.

-}

import Io exposing (HostError, Op)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode



-- THE ALGEBRA


type Step
    = Do Op (List Cleanup)
    | Commit Op


type Cleanup
    = Always Op
    | OnFailure Op


type Continuation
    = Pause
    | Finish


stepOp : Step -> Op
stepOp step =
    case step of
        Do op _ ->
            op

        Commit op ->
            op


cleanupOp : Cleanup -> Op
cleanupOp cleanup =
    case cleanup of
        Always op ->
            op

        OnFailure op ->
            op


isAlways : Cleanup -> Bool
isAlways cleanup =
    case cleanup of
        Always _ ->
            True

        OnFailure _ ->
            False



-- INSTRUCTIONS


{-| `tx` is the transaction's name on the wire. The bridge treats it as an
opaque key; whoever owns the transaction chooses it.
-}
encodeBegin : String -> List Step -> Continuation -> Encode.Value
encodeBegin tx steps continuation =
    Encode.object
        [ ( "do", Encode.string "begin" )
        , ( "tx", Encode.string tx )
        , ( "steps", Encode.list encodeStep steps )
        , ( "then", encodeContinuation continuation )
        ]


encodeExtend : String -> List Step -> Continuation -> Encode.Value
encodeExtend tx steps continuation =
    Encode.object
        [ ( "do", Encode.string "extend" )
        , ( "tx", Encode.string tx )
        , ( "steps", Encode.list encodeStep steps )
        , ( "then", encodeContinuation continuation )
        ]


encodeAbort : String -> Encode.Value
encodeAbort tx =
    Encode.object
        [ ( "do", Encode.string "abort" )
        , ( "tx", Encode.string tx )
        ]


encodeStep : Step -> Encode.Value
encodeStep step =
    case step of
        Do op cleanups ->
            Encode.object
                [ ( "step", Encode.string "do" )
                , ( "op", Io.encodeOp op )
                , ( "cleanups", Encode.list encodeCleanup cleanups )
                ]

        Commit op ->
            Encode.object
                [ ( "step", Encode.string "commit" )
                , ( "op", Io.encodeOp op )
                , ( "cleanups", Encode.list encodeCleanup [] )
                ]


encodeCleanup : Cleanup -> Encode.Value
encodeCleanup cleanup =
    case cleanup of
        Always op ->
            Encode.object [ ( "when", Encode.string "always" ), ( "op", Io.encodeOp op ) ]

        OnFailure op ->
            Encode.object [ ( "when", Encode.string "onFailure" ), ( "op", Io.encodeOp op ) ]


encodeContinuation : Continuation -> Encode.Value
encodeContinuation continuation =
    case continuation of
        Pause ->
            Encode.string "pause"

        Finish ->
            Encode.string "finish"



-- REPORTS


type Status
    = Paused
    | Settled


{-| `steps` holds the outcomes of the steps run since the previous report,
in order. `cleanups` is only ever non-empty in the settled report.
`leaked` names handles the transaction opened and never closed; the bridge
closes them, and their existence is a bug in the plan.
-}
type alias Report =
    { status : Status
    , committed : Bool
    , aborted : Bool
    , steps : List (Result HostError Decode.Value)
    , cleanups : List CleanupResult
    , leaked : List String
    }


type alias CleanupResult =
    { kind : String
    , path : Maybe String
    , outcome : Result HostError Decode.Value
    }


reportDecoder : Decoder Report
reportDecoder =
    Decode.map6 Report
        (Decode.field "status" Decode.string
            |> Decode.andThen
                (\status ->
                    case status of
                        "paused" ->
                            Decode.succeed Paused

                        "settled" ->
                            Decode.succeed Settled

                        _ ->
                            Decode.fail ("unknown transaction status " ++ status)
                )
        )
        (Decode.field "committed" Decode.bool)
        (Decode.field "aborted" Decode.bool)
        (Decode.field "steps" (Decode.list Io.outcomeDecoder))
        (Decode.field "cleanups" (Decode.list cleanupResultDecoder))
        (Decode.field "leaked" (Decode.list Decode.string))


{-| The bridge's report JSON, for feeding the reference machine's reports
to code that expects the bridge.
-}
encodeReport : Report -> Encode.Value
encodeReport r =
    let
        outcome result =
            case result of
                Ok value ->
                    Encode.object [ ( "ok", value ) ]

                Err error ->
                    Encode.object [ ( "error", Io.encodeHostError error ) ]
    in
    Encode.object
        [ ( "status"
          , Encode.string
                (case r.status of
                    Paused ->
                        "paused"

                    Settled ->
                        "settled"
                )
          )
        , ( "committed", Encode.bool r.committed )
        , ( "aborted", Encode.bool r.aborted )
        , ( "steps", Encode.list outcome r.steps )
        , ( "cleanups"
          , Encode.list
                (\c ->
                    Encode.object
                        [ ( "kind", Encode.string c.kind )
                        , ( "path", Maybe.withDefault Encode.null (Maybe.map Encode.string c.path) )
                        , ( "outcome", outcome c.outcome )
                        ]
                )
                r.cleanups
          )
        , ( "leaked", Encode.list Encode.string r.leaked )
        ]


cleanupResultDecoder : Decoder CleanupResult
cleanupResultDecoder =
    Decode.map3 CleanupResult
        (Decode.field "kind" Decode.string)
        (Decode.field "path" (Decode.nullable Decode.string))
        (Decode.field "outcome" Io.outcomeDecoder)


{-| The bridge echoes `kind` and `path` from the op JSON it was sent; the
reference reads them from the same encoding, so the two cannot drift.
-}
describe : Op -> { kind : String, path : Maybe String }
describe op =
    let
        json =
            Io.encodeOp op
    in
    { kind = Decode.decodeValue (Decode.field "kind" Decode.string) json |> Result.withDefault ""
    , path = Decode.decodeValue (Decode.field "path" Decode.string) json |> Result.toMaybe
    }



-- REFERENCE SEMANTICS


{-| The interpreter, one event at a time. Tests run Elm's server against it
as a fake bridge, and tests/vm checks that the JavaScript interpreter makes
exactly the same moves on random programs.
-}
type Machine
    = Machine State


type alias State =
    { queue : List Step
    , more : Bool
    , registered : List Cleanup
    , committed : Bool
    , aborted : Bool
    , results : List (Result HostError Decode.Value)
    , phase : Phase
    }


type Phase
    = Running Step
    | Waiting
    | Unwinding (List Cleanup) (List CleanupResult)
    | Done


boot : List Step -> Continuation -> ( Machine, List Report )
boot steps continuation =
    advance
        { queue = steps
        , more = continuation == Pause
        , registered = []
        , committed = False
        , aborted = False
        , results = []
        , phase = Waiting
        }


{-| The primitive the host should be executing right now, if any.
-}
inFlight : Machine -> Maybe Op
inFlight (Machine m) =
    case m.phase of
        Running step ->
            Just (stepOp step)

        Unwinding (cleanup :: _) _ ->
            Just (cleanupOp cleanup)

        Unwinding [] _ ->
            Nothing

        Waiting ->
            Nothing

        Done ->
            Nothing


{-| Whether the in-flight primitive has been told to stop. Only a forward
step before the commit is ever signalled.
-}
signalled : Machine -> Bool
signalled (Machine m) =
    case m.phase of
        Running _ ->
            m.aborted && not m.committed

        Waiting ->
            False

        Unwinding _ _ ->
            False

        Done ->
            False


isSettled : Machine -> Bool
isSettled (Machine m) =
    m.phase == Done


{-| The in-flight primitive finished with this outcome.
-}
land : Result HostError Decode.Value -> Machine -> ( Machine, List Report )
land outcome (Machine m) =
    case m.phase of
        Running step ->
            let
                recorded =
                    { m | results = outcome :: m.results }
            in
            case ( outcome, step ) of
                ( Ok _, Commit _ ) ->
                    advance { recorded | committed = True }

                ( Ok _, Do _ cleanups ) ->
                    advance { recorded | registered = List.reverse cleanups ++ recorded.registered }

                ( Err _, _ ) ->
                    if recorded.committed then
                        advance recorded

                    else
                        unwind recorded

        Unwinding (cleanup :: rest) done ->
            let
                what =
                    describe (cleanupOp cleanup)

                finished =
                    { kind = what.kind, path = what.path, outcome = outcome } :: done
            in
            case rest of
                [] ->
                    settle { m | phase = Unwinding [] finished }

                _ ->
                    ( Machine { m | phase = Unwinding rest finished }, [] )

        Unwinding [] _ ->
            ( Machine m, [] )

        Waiting ->
            ( Machine m, [] )

        Done ->
            ( Machine m, [] )


preempt : Machine -> ( Machine, List Report )
preempt (Machine m) =
    let
        aborted =
            { m | aborted = True }
    in
    case m.phase of
        Waiting ->
            unwind aborted

        Running _ ->
            ( Machine aborted, [] )

        Unwinding _ _ ->
            ( Machine aborted, [] )

        Done ->
            ( Machine m, [] )


append : List Step -> Continuation -> Machine -> ( Machine, List Report )
append steps continuation (Machine m) =
    case m.phase of
        Waiting ->
            advance { m | queue = m.queue ++ steps, more = continuation == Pause }

        Running _ ->
            ( Machine m, [] )

        Unwinding _ _ ->
            ( Machine m, [] )

        Done ->
            ( Machine m, [] )


advance : State -> ( Machine, List Report )
advance m =
    if m.aborted && not m.committed then
        unwind m

    else
        case m.queue of
            step :: rest ->
                ( Machine { m | queue = rest, phase = Running step }, [] )

            [] ->
                if m.more && not m.aborted then
                    ( Machine { m | phase = Waiting, results = [] }
                    , [ report Paused m [] ]
                    )

                else
                    unwind m


unwind : State -> ( Machine, List Report )
unwind m =
    case List.filter (\cleanup -> isAlways cleanup || not m.committed) m.registered of
        [] ->
            settle { m | phase = Unwinding [] [] }

        due ->
            ( Machine { m | phase = Unwinding due [] }, [] )


settle : State -> ( Machine, List Report )
settle m =
    let
        done =
            case m.phase of
                Unwinding _ finished ->
                    List.reverse finished

                _ ->
                    []
    in
    ( Machine { m | phase = Done, results = [] }, [ report Settled m done ] )


report : Status -> State -> List CleanupResult -> Report
report status m cleanups =
    { status = status
    , committed = m.committed
    , aborted = m.aborted
    , steps = List.reverse m.results
    , cleanups = cleanups
    , leaked = []
    }



-- ONE-SHOT RUNS


type AbortPoint
    = DuringStep Int
    | WhilePaused Int


{-| How the host behaves in one run: the outcome of the n-th step and the
n-th cleanup to execute, and where (if anywhere) an abort lands.
-}
type alias Script =
    { step : Int -> Op -> Result HostError Decode.Value
    , cleanup : Int -> Op -> Result HostError Decode.Value
    , abort : Maybe AbortPoint
    }


{-| Run a transaction made of segments (each ends in `Pause` or `Finish`)
to completion against a script, collecting every report.
-}
run : Script -> List ( List Step, Continuation ) -> List Report
run script segments =
    case segments of
        [] ->
            []

        ( steps, continuation ) :: later ->
            let
                ( machine, reports ) =
                    boot steps continuation
            in
            drive script later { steps = 0, cleanups = 0, pauses = 0 } machine reports 10000


drive : Script -> List ( List Step, Continuation ) -> { steps : Int, cleanups : Int, pauses : Int } -> Machine -> List Report -> Int -> List Report
drive script later counts machine reports fuel =
    if fuel <= 0 || isSettled machine then
        reports

    else
        case ( inFlight machine, machine ) of
            ( Just op, Machine m ) ->
                case m.phase of
                    Running _ ->
                        let
                            ( aborting, _ ) =
                                if script.abort == Just (DuringStep counts.steps) then
                                    preempt machine

                                else
                                    ( machine, [] )

                            outcome =
                                if signalled aborting && Io.interruptible op then
                                    Err Io.revokedError

                                else
                                    script.step counts.steps op

                            ( next, emitted ) =
                                land outcome aborting
                        in
                        drive script later { counts | steps = counts.steps + 1 } next (reports ++ emitted) (fuel - 1)

                    _ ->
                        let
                            ( next, emitted ) =
                                land (script.cleanup counts.cleanups op) machine
                        in
                        drive script later { counts | cleanups = counts.cleanups + 1 } next (reports ++ emitted) (fuel - 1)

            ( Nothing, _ ) ->
                let
                    ( next, emitted ) =
                        if script.abort == Just (WhilePaused counts.pauses) then
                            preempt machine

                        else
                            case later of
                                ( steps, continuation ) :: _ ->
                                    append steps continuation machine

                                [] ->
                                    append [] Finish machine

                    remaining =
                        if script.abort == Just (WhilePaused counts.pauses) then
                            later

                        else
                            List.drop 1 later
                in
                drive script remaining { counts | pauses = counts.pauses + 1 } next (reports ++ emitted) (fuel - 1)
