module RouteTest exposing (suite)

import Domain exposing (ItemId)
import Expect
import Fuzz exposing (Fuzzer)
import Http.Request exposing (Method(..), allMethods)
import Http.Route as Route exposing (Mismatch(..), Route(..))
import Test exposing (Test, describe, fuzz, fuzzWith, test)
import Test.Distribution


itemId : Fuzzer ItemId
itemId =
    Fuzz.oneOf
        [ Fuzz.intRange 1 Domain.maxItemId
        , Fuzz.oneOfValues [ 1, 9, 10, 99, 100, Domain.maxItemId ]
        ]
        |> Fuzz.filterMap Domain.itemIdFromInt


route : Fuzzer Route
route =
    Fuzz.oneOf
        [ Fuzz.constant Health
        , Fuzz.map Item itemId
        , Fuzz.constant Echo
        , Fuzz.map Delayed itemId
        , Fuzz.constant Stats
        , Fuzz.map Ingest itemId
        , Fuzz.map Blob itemId
        , Fuzz.map Upload itemId
        ]


{-| Segments chosen to sit near the route table: real names, canonical and
non-canonical integers, empty segments, case and encoding variants.
-}
segment : Fuzzer String
segment =
    Fuzz.oneOf
        [ Fuzz.oneOfValues
            [ "health"
            , "items"
            , "echo"
            , "delayed"
            , "stats"
            , "Health"
            , ""
            , "0"
            , "7"
            , "42"
            , "007"
            , "+7"
            , "-7"
            , "-0"
            , "1e3"
            , " 7"
            , "%37"
            , "2147483647"
            , "2147483648"
            , "99999999999999999999"
            , "abc"
            ]
        , Fuzz.map String.fromInt (Fuzz.intRange -5 3000000000)
        , Fuzz.asciiString
        ]


{-| Weighted toward two-segment paths under real prefixes, so that a useful
fraction of generated lines parse at all; the distribution check in the suite
fails the run if that fraction collapses and the law goes vacuous.
-}
wirePath : Fuzzer String
wirePath =
    Fuzz.frequency
        [ ( 4
          , Fuzz.map2 (\prefix seg -> "/" ++ prefix ++ "/" ++ seg)
                (Fuzz.oneOfValues [ "items", "delayed", "ingest", "blobs", "health", "Items" ])
                segment
          )
        , ( 2, Fuzz.map (\seg -> "/" ++ seg) segment )
        , ( 2, Fuzz.map (\segments -> "/" ++ String.join "/" segments) (Fuzz.listOfLengthBetween 0 4 segment) )
        , ( 1, Fuzz.asciiString )
        ]


method : Fuzzer Method
method =
    Fuzz.frequency
        [ ( 3, Fuzz.constant Get )
        , ( 1, Fuzz.oneOfValues allMethods )
        ]


parses : ( Method, String ) -> Bool
parses ( m, p ) =
    case Route.parse m p of
        Ok _ ->
            True

        Err _ ->
            False


{-| Compare rejections by position and segment; the expectation text is
prose and free to change.
-}
expectRejection : Method -> String -> Mismatch -> Expect.Expectation
expectRejection m p expected =
    let
        shape mismatch =
            case mismatch of
                InvalidSegment e ->
                    InvalidSegment { e | expected = "" }

                other ->
                    other
    in
    case Route.parse m p of
        Ok r ->
            Expect.fail ("expected " ++ p ++ " to be rejected, got " ++ Debug.toString r)

        Err actual ->
            shape actual |> Expect.equal (shape expected)


bad : Int -> String -> Mismatch
bad position seg =
    InvalidSegment { position = position, segment = seg, expected = "" }


suite : Test
suite =
    describe "Http.Route"
        [ describe "bijection laws"
            [ fuzz route "parse . format == Ok" <|
                \r ->
                    let
                        wire =
                            Route.format r
                    in
                    Route.parse wire.method wire.path |> Expect.equal (Ok r)
            , fuzzWith
                { runs = 5000
                , distribution =
                    Test.expectDistribution
                        [ ( Test.Distribution.atLeast 2, "parses", parses ) ]
                }
                (Fuzz.pair method wirePath)
                "format . parse == id wherever parse succeeds"
              <|
                \( m, p ) ->
                    case Route.parse m p of
                        Ok r ->
                            Route.format r |> Expect.equal { method = m, path = p }

                        Err _ ->
                            Expect.pass
            ]
        , describe "accepts"
            [ test "/items/123" <|
                \_ ->
                    Route.parse Get "/items/123"
                        |> Expect.equal (Domain.itemIdFromInt 123 |> Maybe.map Item |> Result.fromMaybe NoSuchPath)
            , test "max id" <|
                \_ ->
                    Route.parse Get "/delayed/2147483647"
                        |> Expect.equal (Domain.itemIdFromInt 2147483647 |> Maybe.map Delayed |> Result.fromMaybe NoSuchPath)
            ]
        , describe "rejects non-canonical parameters with 400"
            (List.map (\seg -> test ("/items/" ++ seg) <| \_ -> expectRejection Get ("/items/" ++ seg) (bad 1 seg))
                [ "abc", "007", "+7", "-7", "-0", "0", "1e3", " 7", "%37", "2147483648", "99999999999999999999", "" ]
            )
        , describe "rejects unknown shapes with 404"
            (List.map (\p -> test (Debug.toString p) <| \_ -> expectRejection Get p NoSuchPath)
                [ "", "/", "health", "/health/", "//health", "/Health", "/items", "/items/7/", "/items/7/x", "*", "http://localhost/health" ]
            )
        , describe "known paths under the wrong method are 405 with the allowed set"
            [ test "PUT /health" <| \_ -> expectRejection Put "/health" (MethodNotAllowed [ Get ])
            , test "GET /echo" <| \_ -> expectRejection Get "/echo" (MethodNotAllowed [ Post ])
            , test "POST /items/7" <| \_ -> expectRejection Post "/items/7" (MethodNotAllowed [ Get ])
            , test "HEAD /stats" <| \_ -> expectRejection Head "/stats" (MethodNotAllowed [ Get ])
            , test "a bad parameter outranks the method" <| \_ -> expectRejection Post "/items/abc" (bad 1 "abc")
            ]
        ]
