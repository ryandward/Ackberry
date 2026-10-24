module KernelTest exposing (suite)

import Expect
import Http.Request as Request
import Http.Route as Route
import Io exposing (HostError)
import Json.Decode as Decode
import Json.Encode as Encode
import Kernel exposing (Closing(..), Release(..), WhenSpent(..), Witness(..))
import Layout
import Test exposing (Test, describe, test)


hostError : String -> HostError
hostError code =
    Decode.decodeValue Io.hostErrorDecoder
        (Encode.object
            [ ( "name", Encode.string "Error" )
            , ( "code", Encode.string code )
            , ( "syscall", Encode.string "lstat" )
            , ( "rawMessage", Encode.string code )
            , ( "cause", Encode.null )
            ]
        )
        |> Result.withDefault Io.revokedError


bounds : { deadlineMs : Int, budgetBytes : Int }
bounds =
    { deadlineMs = 500, budgetBytes = 131072 }


conn : String -> Request.ConnId
conn =
    Request.connIdFromString


{-| Dissipate one connection after another into a pool of `cap`.
-}
fill : Int -> List String -> ( List String, Kernel.Pool )
fill cap ids =
    List.foldl
        (\id ( shown, p ) ->
            let
                ( closing, next ) =
                    Kernel.dissipate bounds (conn id) p
            in
            ( shown
                ++ [ case closing of
                        Dissipating d ->
                            case d.whenSpent of
                                Hold ->
                                    "hold"

                                Cut ->
                                    "cut"

                        Severed ->
                            "severed"
                   ]
            , next
            )
        )
        ( [], Kernel.pool cap )
        ids


suite : Test
suite =
    describe "Kernel"
        [ test "hold while half the pool would stay free, cut under pressure, sever when full" <|
            \_ ->
                fill 4 [ "a", "b", "c", "d", "e" ]
                    |> Tuple.mapSecond Kernel.held
                    |> Expect.equal ( [ "hold", "hold", "cut", "cut", "severed" ], 4 )
        , test "a release frees exactly the connection named, and only once" <|
            \_ ->
                let
                    ( _, full ) =
                        fill 4 [ "a", "b" ]
                in
                Expect.equal
                    ( Just 1, Nothing, Nothing )
                    ( Kernel.released (conn "a") full |> Maybe.map Kernel.held
                    , Kernel.released (conn "a") full |> Maybe.andThen (Kernel.released (conn "a"))
                    , Kernel.released (conn "never-dissipated") full
                    )
        , test "a severed connection holds no slot" <|
            \_ ->
                fill 1 [ "a", "b" ]
                    |> Tuple.second
                    |> Kernel.released (conn "b")
                    |> Expect.equal Nothing
        , test "the bridge's names for how a dissipation ended, and nothing else" <|
            \_ ->
                List.map Kernel.releaseFromString [ "hungUp", "deadline", "budget", "poolFull", "" ]
                    |> Expect.equal [ Just HungUp, Just DeadlinePassed, Just BudgetSpent, Nothing, Nothing ]
        , test "a probe proves presence or absence only through ENOENT; any other failure is not absence" <|
            \_ ->
                Expect.equal
                    [ Ok Present, Ok NotSeen, Err "EACCES" ]
                    (List.map (Kernel.witness >> Result.mapError Io.errorCode)
                        [ Ok (), Err (hostError "ENOENT"), Err (hostError "EACCES") ]
                    )
        , test "a witness can refuse but never grant: NotSeen leaves the decision to link" <|
            \_ ->
                Expect.equal [ True, False ] (List.map (Kernel.refusal >> (/=) Nothing) [ Present, NotSeen ])
        , test "only create-only routes are probed before their body is let in" <|
            \_ ->
                let
                    layout =
                        Layout.fromDataDir "/d"

                    probed route =
                        Kernel.preflight layout route |> Maybe.map Layout.finalPath
                in
                case Route.parse Request.Put "/blobs/9" of
                    Ok upload ->
                        Expect.equal [ Just "/d/blobs/9", Nothing ] [ probed upload, probed Route.Health ]

                    Err _ ->
                        Expect.fail "PUT /blobs/9 does not parse"
        ]
