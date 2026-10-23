module Http.Route exposing
    ( Mismatch(..)
    , Route(..)
    , SegmentError
    , format
    , parse
    )

{-| Bidirectional routing: one table, read in both directions.

    format : Route -> { method, path }               -- total
    parse  : Method -> String -> Result Mismatch Route

Each route is declared exactly once, in `codec`, and that one declaration
produces both its printer and its parser, so the two directions cannot be
edited apart. What the compiler enforces:

  - `format` is exhaustive: the `case` inside `define` must cover every
    constructor of `Route`.
  - The number of `fixed`/`captured` declarations equals the number of
    printer slots in `define`; one too many or too few is a type error.

What it cannot enforce, and tests/RouteTest.elm checks as properties:

    parse (format r).method (format r).path == Ok r

    parse m p == Ok r implies format r == { method = m, path = p }

Together these make `parse` and `format` a bijection between `Route` and
the set of canonical request lines. Non-canonical spellings (`/items/007`,
`/items/+7`, `/health/`, `/items/%37`) are rejected, not normalized, because
normalizing would map two wire strings onto one route and break the
second law.

-}

import Domain exposing (ItemId)
import Http.Request exposing (Method(..))


type Route
    = Health
    | Item ItemId
    | Echo
    | Delayed ItemId
    | Stats
    | Ingest ItemId
    | Blob ItemId
    | Upload ItemId


codec : Codec
codec =
    define
        (\health item echo delayed stats ingest blob upload route ->
            case route of
                Health ->
                    health

                Item id ->
                    item id

                Echo ->
                    echo

                Delayed id ->
                    delayed id

                Stats ->
                    stats

                Ingest id ->
                    ingest id

                Blob id ->
                    blob id

                Upload id ->
                    upload id
        )
        |> fixed Health Get [ "health" ]
        |> captured Item Get [ "items" ] itemId
        |> fixed Echo Post [ "echo" ]
        |> captured Delayed Get [ "delayed" ] itemId
        |> fixed Stats Get [ "stats" ]
        |> captured Ingest Post [ "ingest" ] itemId
        |> captured Blob Get [ "blobs" ] itemId
        |> captured Upload Put [ "blobs" ] itemId
        |> build


{-| Only the canonical decimal spelling is accepted: `String.toInt` alone
would also take "+7", "007" and "-0", and would turn a 20-digit string into
a float that has already lost digits.
-}
itemId : Param ItemId
itemId =
    { parse =
        \segment ->
            String.toInt segment
                |> Maybe.andThen
                    (\n ->
                        if String.fromInt n == segment then
                            Domain.itemIdFromInt n

                        else
                            Nothing
                    )
    , print = Domain.itemIdToInt >> String.fromInt
    , expected =
        "a decimal integer from 1 to "
            ++ String.fromInt Domain.maxItemId
            ++ " with no sign or leading zeros"
    }



-- PARSE AND FORMAT


type Mismatch
    = NoSuchPath
    | MethodNotAllowed (List Method)
    | InvalidSegment SegmentError


{-| `position` indexes the slash-separated segments after the leading slash,
so in `/items/abc` the bad segment is at position 1.
-}
type alias SegmentError =
    { position : Int
    , segment : String
    , expected : String
    }


format : Route -> { method : Method, path : String }
format route =
    let
        (Codec table) =
            codec

        endpoint =
            table.print route
    in
    { method = endpoint.method
    , path = "/" ++ String.join "/" endpoint.segments
    }


{-| A full path match beats a bad parameter, which beats no match. A path
that matches only under other methods is a 405, not a 404.
-}
parse : Method -> String -> Result Mismatch Route
parse method path =
    let
        (Codec table) =
            codec
    in
    if String.startsWith "/" path then
        let
            segments =
                String.split "/" (String.dropLeft 1 path)

            results =
                List.map (\variant -> ( variant.method, variant.match segments )) table.variants

            matched =
                List.filterMap
                    (\( variantMethod, result ) ->
                        case result of
                            Matched route ->
                                Just ( variantMethod, route )

                            Unmatched ->
                                Nothing

                            Rejected _ ->
                                Nothing
                    )
                    results

            rejected =
                List.filterMap
                    (\( _, result ) ->
                        case result of
                            Rejected error ->
                                Just error

                            Matched _ ->
                                Nothing

                            Unmatched ->
                                Nothing
                    )
                    results
        in
        case List.filter (\( variantMethod, _ ) -> variantMethod == method) matched of
            ( _, route ) :: _ ->
                Ok route

            [] ->
                case ( matched, rejected ) of
                    ( [], [] ) ->
                        Err NoSuchPath

                    ( [], error :: _ ) ->
                        Err (InvalidSegment error)

                    ( _ :: _, _ ) ->
                        Err (MethodNotAllowed (List.map Tuple.first matched))

    else
        Err NoSuchPath



-- CODEC MACHINERY


type alias Endpoint =
    { method : Method
    , segments : List String
    }


{-| A path parameter. `print` must emit a non-empty segment with no slash,
and `parse (print a) == Just a`; the round-trip property tests hold it to that.
-}
type alias Param a =
    { parse : String -> Maybe a
    , print : a -> String
    , expected : String
    }


type Match
    = Matched Route
    | Unmatched
    | Rejected SegmentError


type alias Variant =
    { method : Method
    , match : List String -> Match
    }


type Codec
    = Codec
        { print : Route -> Endpoint
        , variants : List Variant
        }


{-| `print` starts as a function with one printer slot per route plus the
route itself. Each declaration fills the next slot; `build` only accepts the
builder once nothing but `Route -> Endpoint` is left.
-}
type Builder print
    = Builder
        { print : print
        , variants : List Variant
        }


define : print -> Builder print
define print =
    Builder { print = print, variants = [] }


fixed : Route -> Method -> List String -> Builder (Endpoint -> rest) -> Builder rest
fixed route method segments (Builder builder) =
    Builder
        { print = builder.print { method = method, segments = segments }
        , variants =
            { method = method
            , match =
                \actual ->
                    if actual == segments then
                        Matched route

                    else
                        Unmatched
            }
                :: builder.variants
        }


captured : (a -> Route) -> Method -> List String -> Param a -> Builder ((a -> Endpoint) -> rest) -> Builder rest
captured toRoute method prefix param (Builder builder) =
    Builder
        { print = builder.print (\a -> { method = method, segments = prefix ++ [ param.print a ] })
        , variants =
            { method = method
            , match =
                \actual ->
                    case stripPrefix prefix actual of
                        Just [ segment ] ->
                            case param.parse segment of
                                Just a ->
                                    Matched (toRoute a)

                                Nothing ->
                                    Rejected
                                        { position = List.length prefix
                                        , segment = segment
                                        , expected = param.expected
                                        }

                        _ ->
                            Unmatched
            }
                :: builder.variants
        }


build : Builder (Route -> Endpoint) -> Codec
build (Builder builder) =
    Codec { print = builder.print, variants = List.reverse builder.variants }


stripPrefix : List String -> List String -> Maybe (List String)
stripPrefix prefix segments =
    case ( prefix, segments ) of
        ( [], rest ) ->
            Just rest

        ( p :: ps, s :: ss ) ->
            if p == s then
                stripPrefix ps ss

            else
                Nothing

        ( _ :: _, [] ) ->
            Nothing
