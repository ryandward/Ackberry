module SerializeTest exposing (suite)

import Expect
import Fuzz
import Http.Date as Date
import Http.Request as Request
import Http.Response as Response exposing (Failure(..))
import Json.Encode as Encode
import Test exposing (Test, describe, fuzz, test)


date : Date.Date
date =
    Date.fromMillis 784111777000


crlf : String
crlf =
    "\u{000D}\n"


{-| The head and the body of a serialized response.
-}
split : String -> ( List String, String )
split text =
    case String.indexes (crlf ++ crlf) text of
        i :: _ ->
            ( String.split crlf (String.left i text), String.dropLeft (i + 4) text )

        [] ->
            ( [], text )


lengthOf : List String -> Maybe Int
lengthOf lines =
    lines
        |> List.filterMap
            (\line ->
                case String.split ": " line of
                    [ "content-length", n ] ->
                        String.toInt n

                    _ ->
                        Nothing
            )
        |> List.head


failures : List Failure
failures =
    [ BadRequest, NotFound, MethodNotAllowed [ Request.Get ], Conflict, RequestTimeout, PayloadTooLarge, UriTooLong, ExpectationFailed, HeaderFieldsTooLarge, InternalServerError, NotImplemented, BadGateway, ServiceUnavailable, GatewayTimeout, VersionNotSupported, InsufficientStorage ]


suite : Test
suite =
    describe "serialization"
        [ test "IMF-fixdate, floored to the second, across epochs, a leap day and the year 9999" <|
            \_ ->
                List.map (Date.fromMillis >> Date.toString) [ 0, 784111777000, 784111777999, 951782400000, 253402300799000 ]
                    |> Expect.equal
                        [ "Thu, 01 Jan 1970 00:00:00 GMT"
                        , "Sun, 06 Nov 1994 08:49:37 GMT"
                        , "Sun, 06 Nov 1994 08:49:37 GMT"
                        , "Tue, 29 Feb 2000 00:00:00 GMT"
                        , "Fri, 31 Dec 9999 23:59:59 GMT"
                        ]
        , test "UTF-8 octets, not characters: 1, 2, 3 and 4-byte code points" <|
            \_ ->
                List.map Response.utf8Length [ "", "abc", "é", "€", "😀", "aé€😀" ]
                    |> Expect.equal [ 0, 3, 2, 3, 4, 10 ]
        , test "a response: status line, fields, date, then exactly the body, framed by its octet count" <|
            \_ ->
                Response.serialize date False (Response.ok (Encode.object [ ( "name", Encode.string "café €" ) ]))
                    |> split
                    |> Expect.equal
                        ( [ "HTTP/1.1 200 OK"
                          , "content-type: application/json; charset=utf-8"
                          , "content-length: 20"
                          , "date: Sun, 06 Nov 1994 08:49:37 GMT"
                          ]
                        , "{\"name\":\"café €\"}"
                        )
        , fuzz Fuzz.string "the declared length is the body's UTF-8 length, whatever the body" <|
            \text ->
                let
                    ( lines, body ) =
                        split (Response.serialize date False (Response.ok (Encode.string text)))
                in
                Expect.equal (Just (Response.utf8Length body)) (lengthOf lines)
        , test "connection: close only when the connection ends" <|
            \_ ->
                List.map (\closes -> Response.serialize date closes (Response.problem BadRequest "no" []) |> split |> Tuple.first |> List.member "connection: close") [ False, True ]
                    |> Expect.equal [ False, True ]
        , test "every failure's status line carries its number and its phrase; a 405 carries Allow" <|
            \_ ->
                List.map
                    (\failure ->
                        Response.serialize date False (Response.problem failure "x" []) |> split |> Tuple.first |> List.head |> Maybe.withDefault ""
                    )
                    failures
                    |> Expect.equal
                        [ "HTTP/1.1 400 Bad Request"
                        , "HTTP/1.1 404 Not Found"
                        , "HTTP/1.1 405 Method Not Allowed"
                        , "HTTP/1.1 409 Conflict"
                        , "HTTP/1.1 408 Request Timeout"
                        , "HTTP/1.1 413 Content Too Large"
                        , "HTTP/1.1 414 URI Too Long"
                        , "HTTP/1.1 417 Expectation Failed"
                        , "HTTP/1.1 431 Request Header Fields Too Large"
                        , "HTTP/1.1 500 Internal Server Error"
                        , "HTTP/1.1 501 Not Implemented"
                        , "HTTP/1.1 502 Bad Gateway"
                        , "HTTP/1.1 503 Service Unavailable"
                        , "HTTP/1.1 504 Gateway Timeout"
                        , "HTTP/1.1 505 HTTP Version Not Supported"
                        , "HTTP/1.1 507 Insufficient Storage"
                        ]
        , test "a 405 names the methods it allows" <|
            \_ ->
                Response.serialize date False (Response.problem (MethodNotAllowed [ Request.Get, Request.Put ]) "x" [])
                    |> split
                    |> Tuple.first
                    |> List.member "allow: GET, PUT"
                    |> Expect.equal True
        , test "100 Continue is exactly the interim status line and an empty line" <|
            \_ -> Expect.equal ("HTTP/1.1 100 Continue" ++ crlf ++ crlf) Response.continue
        , test "a download's head declares the file's exact length and nothing follows it" <|
            \_ ->
                Response.serializeStreamHead date False 10485760
                    |> Expect.equal
                        (String.join crlf
                            [ "HTTP/1.1 200 OK"
                            , "content-type: application/octet-stream"
                            , "content-length: 10485760"
                            , "date: Sun, 06 Nov 1994 08:49:37 GMT"
                            , ""
                            , ""
                            ]
                        )
        ]
