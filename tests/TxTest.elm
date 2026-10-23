module TxTest exposing (suite)

{-| The reference interpreter in `Tx` is incremental, because the bridge is.
Here the laws are restated as one direct fold over the program, and the two
must agree on random programs, outcomes and abort points.
-}

import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Io exposing (HostError, Op)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (Test, describe, fuzzWith, test)
import Test.Distribution
import Tx



-- PROGRAMS


type alias StepShape =
    { commit : Bool
    , interruptible : Bool
    , cleanups : List ( Bool, Bool )
    }


type alias Program =
    { segments : List ( List Tx.Step, Tx.Continuation )
    , outcomes : Dict String (Result HostError Decode.Value)
    , abort : Maybe Tx.AbortPoint
    }


stepShape : Fuzzer StepShape
stepShape =
    Fuzz.map3 StepShape
        (Fuzz.weightedBool 0.15)
        (Fuzz.weightedBool 0.3)
        (Fuzz.listOfLengthBetween 0 2 (Fuzz.pair Fuzz.bool (Fuzz.weightedBool 0.2)))


outcomeFuzzer : Fuzzer (Result HostError Decode.Value)
outcomeFuzzer =
    Fuzz.frequency
        [ ( 4, Fuzz.constant (Ok (Encode.object [])) )
        , ( 1, Fuzz.map (\code -> Err (hostError code)) (Fuzz.oneOfValues [ "EIO", "ENOSPC", "EACCES", "EWEIRD" ]) )
        ]


hostError : String -> HostError
hostError code =
    Decode.decodeValue Io.hostErrorDecoder
        (Encode.object
            [ ( "name", Encode.string "Error" )
            , ( "code", Encode.string code )
            , ( "syscall", Encode.string "" )
            , ( "rawMessage", Encode.string code )
            , ( "cause", Encode.null )
            ]
        )
        |> Result.withDefault Io.revokedError


program : Fuzzer Program
program =
    Fuzz.map3
        (\shapes outcomes abort ->
            { segments = label shapes
            , outcomes = Dict.fromList (List.indexedMap (\i o -> ( "s" ++ String.fromInt i, o )) outcomes ++ List.indexedMap (\i o -> ( "c" ++ String.fromInt i, o )) (List.reverse outcomes))
            , abort = abort
            }
        )
        (Fuzz.listOfLengthBetween 1 3 (Fuzz.listOfLengthBetween 0 4 stepShape))
        (Fuzz.listOfLength 30 outcomeFuzzer)
        (Fuzz.oneOf
            [ Fuzz.constant Nothing
            , Fuzz.map (Just << Tx.DuringStep) (Fuzz.intRange 0 10)
            , Fuzz.map (Just << Tx.WhilePaused) (Fuzz.intRange 0 2)
            ]
        )


{-| Give every op a unique label: steps s0, s1, ... and cleanups c0, c1, ...
Outcomes are looked up by label, as the JavaScript harness does too.
-}
label : List (List StepShape) -> List ( List Tx.Step, Tx.Continuation )
label shapes =
    let
        op interruptible name =
            if interruptible then
                Io.HttpGet { url = name, maxBytes = 1 }

            else
                Io.Unlink name

        build shape ( n, c, acc ) =
            let
                cleanups =
                    List.indexedMap
                        (\i ( always, interruptible ) ->
                            if always then
                                Tx.Always (op interruptible ("c" ++ String.fromInt (c + i)))

                            else
                                Tx.OnFailure (op interruptible ("c" ++ String.fromInt (c + i)))
                        )
                        shape.cleanups

                stepOp =
                    op shape.interruptible ("s" ++ String.fromInt n)

                built =
                    if shape.commit then
                        Tx.Commit stepOp

                    else
                        Tx.Do stepOp cleanups
            in
            ( n + 1, c + List.length shape.cleanups, acc ++ [ built ] )

        ( _, _, segments ) =
            List.foldl
                (\segment ( n, c, done ) ->
                    let
                        ( n2, c2, steps ) =
                            List.foldl build ( n, c, [] ) segment
                    in
                    ( n2, c2, done ++ [ steps ] )
                )
                ( 0, 0, [] )
                shapes

        count =
            List.length segments
    in
    List.indexedMap
        (\i steps ->
            ( steps
            , if i == count - 1 then
                Tx.Finish

              else
                Tx.Pause
            )
        )
        segments


labelOf : Op -> String
labelOf op =
    case op of
        Io.Unlink name ->
            name

        Io.HttpGet get ->
            get.url

        _ ->
            ""


script : Program -> Tx.Script
script p =
    let
        lookup _ op =
            Dict.get (labelOf op) p.outcomes |> Maybe.withDefault (Ok (Encode.object []))
    in
    { step = lookup, cleanup = lookup, abort = p.abort }



-- THE LAWS, DIRECTLY


type alias Expected =
    { results : List (Result HostError Decode.Value)
    , cleanups : List ( String, Maybe String, Result HostError Decode.Value )
    , committed : Bool
    , aborted : Bool
    , pauses : Int
    }


type alias Run =
    { ran : Int
    , registered : List Tx.Cleanup
    , committed : Bool
    , aborted : Bool
    , stopped : Bool
    , results : List (Result HostError Decode.Value)
    , pauses : Int
    }


direct : Program -> Expected
direct p =
    let
        s =
            script p

        stepOp step =
            case step of
                Tx.Do op _ ->
                    op

                Tx.Commit op ->
                    op

        cleanupOp cleanup =
            case cleanup of
                Tx.Always op ->
                    op

                Tx.OnFailure op ->
                    op

        isAlways cleanup =
            case cleanup of
                Tx.Always _ ->
                    True

                Tx.OnFailure _ ->
                    False

        -- Laws 1 to 4: one at a time, register on success, stop at the first
        -- failure or abort before the commit, never stop after it.
        runStep step r =
            if r.stopped then
                r

            else if r.aborted && not r.committed then
                { r | stopped = True }

            else
                let
                    abortNow =
                        p.abort == Just (Tx.DuringStep r.ran)

                    op =
                        stepOp step

                    outcome =
                        if abortNow && not r.committed && Io.interruptible op then
                            Err Io.revokedError

                        else
                            s.step r.ran op

                    next =
                        { r | ran = r.ran + 1, aborted = r.aborted || abortNow, results = r.results ++ [ outcome ] }
                in
                case ( outcome, step ) of
                    ( Ok _, Tx.Commit _ ) ->
                        { next | committed = True, registered = List.filter isAlways next.registered }

                    ( Ok _, Tx.Do _ cleanups ) ->
                        { next | registered = next.registered ++ cleanups }

                    ( Err _, _ ) ->
                        if next.committed then
                            next

                        else
                            { next | stopped = True }

        -- Law 7: a pause hands control back; an abort while paused ends the
        -- transaction, committed or not.
        loop segments r =
            case segments of
                [] ->
                    r

                ( steps, continuation ) :: rest ->
                    let
                        afterSteps =
                            List.foldl runStep r steps
                    in
                    -- An aborted transaction never pauses again: committed or
                    -- not, it ends once the steps already queued are done.
                    if afterSteps.stopped || afterSteps.aborted then
                        afterSteps

                    else
                        case continuation of
                            Tx.Finish ->
                                afterSteps

                            Tx.Pause ->
                                let
                                    paused =
                                        { afterSteps | pauses = afterSteps.pauses + 1 }
                                in
                                if p.abort == Just (Tx.WhilePaused afterSteps.pauses) then
                                    { paused | aborted = True }

                                else
                                    loop rest paused

        final =
            loop p.segments { ran = 0, registered = [], committed = False, aborted = False, stopped = False, results = [], pauses = 0 }

        -- Law 5: due cleanups, newest first, never signalled.
        due =
            List.reverse (List.filter (\c -> isAlways c || not final.committed) final.registered)
    in
    { results = final.results
    , cleanups =
        List.indexedMap
            (\i c ->
                let
                    op =
                        cleanupOp c
                in
                ( kindOf op
                , pathOf op
                , s.cleanup i op
                )
            )
            due
    , committed = final.committed
    , aborted = final.aborted
    , pauses = final.pauses
    }


kindOf : Op -> String
kindOf op =
    case op of
        Io.Unlink _ ->
            "unlink"

        Io.HttpGet _ ->
            "httpGet"

        _ ->
            "?"


pathOf : Op -> Maybe String
pathOf op =
    case op of
        Io.Unlink path ->
            Just path

        _ ->
            Nothing


agrees : Program -> Expect.Expectation
agrees p =
    let
        reports =
            Tx.run (script p) p.segments

        expected =
            direct p

        settled =
            List.filter (\r -> r.status == Tx.Settled) reports
    in
    Expect.all
        [ \_ -> Expect.equal 1 (List.length settled) |> Expect.onFail "law 6: exactly one settled report"
        , \_ -> Expect.equal (Just Tx.Settled) (List.reverse reports |> List.head |> Maybe.map .status) |> Expect.onFail "law 6: the settled report is the last"
        , \_ -> Expect.equal expected.results (List.concatMap .steps reports) |> Expect.onFail "laws 1-4: which steps ran, and how they ended"
        , \_ ->
            Expect.equal expected.cleanups
                (List.concatMap .cleanups settled |> List.map (\c -> ( c.kind, c.path, c.outcome )))
                |> Expect.onFail "law 5: due cleanups, newest first"
        , \_ -> Expect.equal ( expected.committed, expected.aborted ) (List.head settled |> Maybe.map (\r -> ( r.committed, r.aborted )) |> Maybe.withDefault ( False, False ))
        , \_ -> Expect.equal expected.pauses (List.length (List.filter (\r -> r.status == Tx.Paused) reports)) |> Expect.onFail "law 7: one paused report per pause"
        ]
        ()


hasAbort : Program -> Bool
hasAbort p =
    p.abort /= Nothing


commits : Program -> Bool
commits p =
    (direct p).committed


suite : Test
suite =
    describe "Tx"
        [ fuzzWith
            { runs = 3000
            , distribution =
                Test.expectDistribution
                    [ ( Test.Distribution.atLeast 20, "committed", commits )
                    , ( Test.Distribution.atLeast 30, "aborted somewhere", hasAbort )
                    , ( Test.Distribution.atLeast 5, "ran a cleanup", \p -> not (List.isEmpty (direct p).cleanups) )
                    ]
            }
            program
            "the incremental interpreter obeys the laws on random programs"
            agrees
        , test "a failed Open registers nothing: rollback can never touch a file it did not create" <|
            \_ ->
                Tx.run
                    { step = \_ _ -> Err (hostError "EEXIST"), cleanup = \_ _ -> Ok Encode.null, abort = Nothing }
                    [ ( [ Tx.Do (Io.Unlink "open") [ Tx.OnFailure (Io.Unlink "tmp") ] ], Tx.Finish ) ]
                    |> List.concatMap .cleanups
                    |> Expect.equal []
        , test "an abort during an uninterruptible step waits for it, then undoes what it did" <|
            \_ ->
                Tx.run
                    { step = \_ _ -> Ok Encode.null, cleanup = \_ _ -> Ok Encode.null, abort = Just (Tx.DuringStep 0) }
                    [ ( [ Tx.Do (Io.Unlink "open") [ Tx.OnFailure (Io.Unlink "tmp") ], Tx.Do (Io.Unlink "never") [] ], Tx.Finish ) ]
                    |> (\reports -> ( List.concatMap .steps reports |> List.length, List.concatMap .cleanups reports |> List.map .path ))
                    |> Expect.equal ( 1, [ Just "tmp" ] )
        , test "an abort during an interruptible step severs it" <|
            \_ ->
                Tx.run
                    { step = \_ _ -> Ok Encode.null, cleanup = \_ _ -> Ok Encode.null, abort = Just (Tx.DuringStep 1) }
                    [ ( [ Tx.Do (Io.Unlink "open") [ Tx.Always (Io.Unlink "close") ], Tx.Do (Io.HttpGet { url = "pipe", maxBytes = 1 }) [] ], Tx.Finish ) ]
                    |> List.concatMap .steps
                    |> List.map (Result.mapError Io.errorCode)
                    |> Expect.equal [ Ok Encode.null, Err "ABORT_ERR" ]
        , test "after the commit an abort stops nothing" <|
            \_ ->
                Tx.run
                    { step = \_ _ -> Ok Encode.null, cleanup = \_ _ -> Ok Encode.null, abort = Just (Tx.DuringStep 1) }
                    [ ( [ Tx.Do (Io.Unlink "a") [ Tx.OnFailure (Io.Unlink "undo"), Tx.Always (Io.Unlink "release") ], Tx.Commit (Io.Unlink "link"), Tx.Do (Io.Unlink "after") [] ], Tx.Finish ) ]
                    |> (\reports -> ( List.concatMap .steps reports |> List.length, List.concatMap .cleanups reports |> List.map .path ))
                    |> Expect.equal ( 3, [ Just "release" ] )
        ]
