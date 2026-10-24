module KernelTest exposing (suite)

import Expect
import Http.Request as Request
import Http.Response as Response
import Http.Route as Route
import Io exposing (HostError)
import Json.Decode as Decode
import Json.Encode as Encode
import Kernel exposing (Arrival(..), Witness(..))
import Layout
import Plan
import Test exposing (Test, describe, test)


request : Request.Declared -> Request.Request
request body =
    { method = Request.Put, path = "/blobs/9", query = Nothing, headers = [], body = body, expectsContinue = False }


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


unread : String -> Kernel.Exchange Kernel.Unread
unread id =
    case Kernel.arrive (Request.idFromString id) (request (Request.Length 10)) of
        Bodied exchange ->
            exchange

        Bodiless _ ->
            Debug.todo "a declared body of 10 bytes arrived bodiless"


profile : Kernel.Dissipation
profile =
    { deadlineMs = 500, budgetBytes = 131072, whenSpent = Kernel.Cut }


refused : Response.Response
refused =
    Response.problem Response.Conflict "taken" []


{-| The fate the bridge would be told, read back from the encoded trace.
-}
fateOf : Kernel.Answer -> String
fateOf given =
    Kernel.respondAfter (Plan.end Plan.fresh) given
        |> Kernel.encode
        |> Decode.decodeValue (Decode.at [ "ending", "fate", "kind" ] Decode.string)
        |> Result.withDefault "undecodable"


suite : Test
suite =
    describe "Kernel"
        [ test "a declared length of zero is a drained exchange; any other body is unread" <|
            \_ ->
                List.map
                    (\body ->
                        case Kernel.arrive (Request.idFromString "a") (request body) of
                            Bodiless _ ->
                                "bodiless"

                            Bodied _ ->
                                "bodied"
                    )
                    [ Request.NoBody, Request.Length 0, Request.Length 1, Request.Unsized ]
                    |> Expect.equal [ "bodiless", "bodiless", "bodied", "bodied" ]
        , test "the pool dissipates up to its cap, then severs; a release frees exactly one slot" <|
            \_ ->
                let
                    step id ( soFar, p ) =
                        let
                            ( given, next ) =
                                Kernel.answerAndClose profile refused (unread id) p
                        in
                        ( soFar ++ [ fateOf given ], next )

                    ( fates, full ) =
                        List.foldl step ( [], Kernel.pool 2 ) [ "a", "b", "c" ]

                    afterRelease =
                        Kernel.released (Request.idFromString "a") full
                in
                Expect.equal
                    ( [ "dissipate", "dissipate", "sever" ], 2, Just 1 )
                    ( fates, Kernel.held full, Maybe.map Kernel.held afterRelease )
        , test "a release for an exchange that holds no slot is refused, never ignored" <|
            \_ ->
                let
                    ( _, onlyA ) =
                        Kernel.answerAndClose profile refused (unread "a") (Kernel.pool 4)
                in
                Expect.equal
                    ( Nothing, Nothing )
                    ( Kernel.released (Request.idFromString "never-dissipated") onlyA
                    , Kernel.released (Request.idFromString "a") onlyA |> Maybe.andThen (Kernel.released (Request.idFromString "a"))
                    )
        , test "only an unread exchange closes: the encoded fate and its connection header agree" <|
            \_ ->
                let
                    headersOf given =
                        Kernel.respondAfter (Plan.end Plan.fresh) given
                            |> Kernel.encode
                            |> Decode.decodeValue (Decode.at [ "ending", "headers" ] (Decode.list Decode.string))
                            |> Result.map (List.member "close")

                    open =
                        case Kernel.arrive (Request.idFromString "g") (request Request.NoBody) of
                            Bodiless exchange ->
                                Kernel.answer (Response.ok (Encode.object [])) exchange

                            Bodied exchange ->
                                Tuple.first (Kernel.answerAndClose profile refused exchange (Kernel.pool 1))

                    ( closing, _ ) =
                        Kernel.answerAndClose profile (Response.ok (Encode.object [])) (unread "h") (Kernel.pool 1)
                in
                Expect.equal
                    ( ( "keepAlive", Ok False ), ( "dissipate", Ok True ) )
                    ( ( fateOf open, headersOf open ), ( fateOf closing, headersOf closing ) )
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
                        Expect.equal
                            [ Just "/d/blobs/9", Nothing ]
                            [ probed upload, probed Route.Health ]

                    Err _ ->
                        Expect.fail "PUT /blobs/9 does not parse"
        ]
