module HeadTest exposing (suite)

import Expect
import Fuzz exposing (Fuzzer)
import Http.Head as Head exposing (Persistence(..), Problem(..), Version(..))
import Http.Request as Request exposing (Framing(..))
import Test exposing (Test, describe, fuzz, test)


limits : Head.Limits
limits =
    { maxHeadBytes = 16384, maxTarget = 8192, maxFields = 100 }


{-| A head as the bridge delivers it: CRLF between lines, an empty line at
the end.
-}
head : List String -> String
head lines =
    String.join "\u{000D}\n" lines ++ "\u{000D}\n\u{000D}\n"


parsed : List String -> Result Problem Head.Head
parsed =
    head >> Head.parse limits


problem : List String -> Maybe Problem
problem lines =
    case parsed lines of
        Ok _ ->
            Nothing

        Err p ->
            Just p


framing : List String -> Result Problem Framing
framing =
    parsed >> Result.map .framing


{-| Each case: the lines of a head and the problem it must be refused with.
-}
refusals : List ( String, List String, Problem )
refusals =
    [ ( "two spaces in the request line", [ "GET /health  HTTP/1.1", "Host: x" ], MalformedRequestLine )
    , ( "a request line without a version", [ "GET /health", "Host: x" ], MalformedRequestLine )
    , ( "a lowercase method is a token nobody implements", [ "get /health HTTP/1.1", "Host: x" ], UnsupportedMethod "get" )
    , ( "an unknown method", [ "FROB / HTTP/1.1", "Host: x" ], UnsupportedMethod "FROB" )
    , ( "a fragment in the target", [ "GET /a#b HTTP/1.1", "Host: x" ], MalformedRequestLine )
    , ( "a relative target", [ "GET health HTTP/1.1", "Host: x" ], MalformedRequestLine )
    , ( "major version 2", [ "GET / HTTP/2.0", "Host: x" ], UnsupportedVersion )
    , ( "a two-digit minor version", [ "GET / HTTP/1.10", "Host: x" ], MalformedRequestLine )
    , ( "a lowercase protocol name", [ "GET / http/1.1", "Host: x" ], MalformedRequestLine )
    , ( "HTTP/1.1 without Host", [ "GET / HTTP/1.1" ], MissingHost )
    , ( "two Host lines", [ "GET / HTTP/1.1", "Host: x", "Host: y" ], MultipleHosts )
    , ( "obs-fold", [ "GET / HTTP/1.1", "Host: x", "X-A: 1", " continued" ], MalformedField )
    , ( "whitespace before the colon", [ "GET / HTTP/1.1", "Host : x" ], MalformedField )
    , ( "a field line without a colon", [ "GET / HTTP/1.1", "Host: x", "nonsense" ], MalformedField )
    , ( "an empty field name", [ "GET / HTTP/1.1", "Host: x", ": value" ], MalformedField )
    , ( "NUL in a value", [ "GET / HTTP/1.1", "Host: x", "X-A: a\u{0000}b" ], MalformedField )
    , ( "DEL in a value", [ "GET / HTTP/1.1", "Host: x", "X-A: a\u{007F}b" ], MalformedField )
    , ( "a bare CR in a value", [ "GET / HTTP/1.1", "Host: x", "X-A: a\u{000D}b" ], MalformedField )
    , ( "Content-Length that is not digits", [ "POST / HTTP/1.1", "Host: x", "Content-Length: +10" ], InvalidContentLength )
    , ( "a negative Content-Length", [ "POST / HTTP/1.1", "Host: x", "Content-Length: -1" ], InvalidContentLength )
    , ( "an empty Content-Length", [ "POST / HTTP/1.1", "Host: x", "Content-Length:" ], InvalidContentLength )
    , ( "Content-Lengths that disagree, in one line", [ "POST / HTTP/1.1", "Host: x", "Content-Length: 10, 11" ], InvalidContentLength )
    , ( "Content-Lengths that disagree, in two lines", [ "POST / HTTP/1.1", "Host: x", "Content-Length: 10", "Content-Length: 11" ], InvalidContentLength )
    , ( "a Content-Length no Int holds exactly", [ "POST / HTTP/1.1", "Host: x", "Content-Length: 12345678901234567890" ], ContentLengthTooLarge )
    , ( "Content-Length beside Transfer-Encoding", [ "POST / HTTP/1.1", "Host: x", "Content-Length: 4", "Transfer-Encoding: chunked" ], AmbiguousLength )
    , ( "Transfer-Encoding in HTTP/1.0", [ "POST / HTTP/1.0", "Transfer-Encoding: chunked" ], FaultyFraming )
    , ( "chunked that is not the last coding", [ "POST / HTTP/1.1", "Host: x", "Transfer-Encoding: chunked, gzip" ], FaultyFraming )
    , ( "chunked twice", [ "POST / HTTP/1.1", "Host: x", "Transfer-Encoding: chunked, chunked" ], FaultyFraming )
    , ( "chunked with a parameter", [ "POST / HTTP/1.1", "Host: x", "Transfer-Encoding: chunked;x=1" ], FaultyFraming )
    , ( "an empty Transfer-Encoding", [ "POST / HTTP/1.1", "Host: x", "Transfer-Encoding:" ], FaultyFraming )
    , ( "a coding this server cannot undo", [ "POST / HTTP/1.1", "Host: x", "Transfer-Encoding: gzip, chunked" ], UnsupportedCoding )
    , ( "codings split across lines are one list", [ "POST / HTTP/1.1", "Host: x", "Transfer-Encoding: gzip", "Transfer-Encoding: chunked" ], UnsupportedCoding )
    , ( "an expectation other than 100-continue", [ "PUT / HTTP/1.1", "Host: x", "Expect: 200-ok" ], UnsupportedExpectation )
    , ( "the asterisk for anything but OPTIONS", [ "GET * HTTP/1.1", "Host: x" ], MalformedRequestLine )
    , ( "CONNECT, which nothing here implements", [ "CONNECT h:443 HTTP/1.1", "Host: h:443" ], UnsupportedMethod "CONNECT" )
    , ( "a method longer than any, not echoed whole", [ String.repeat 5000 "A" ++ " / HTTP/1.1", "Host: x" ], UnsupportedMethod (String.repeat 32 "A") )
    , ( "a Host with a space", [ "GET / HTTP/1.1", "Host: a b" ], InvalidHost )
    , ( "a Host with a path", [ "GET / HTTP/1.1", "Host: evil.com/../x" ], InvalidHost )
    , ( "a Host with a comma, as two folded Hosts look", [ "GET / HTTP/1.1", "Host: a,b" ], InvalidHost )
    , ( "a Host port past 65535", [ "GET / HTTP/1.1", "Host: x:99999" ], InvalidHost )
    , ( "a Host port that is not digits", [ "GET / HTTP/1.1", "Host: x:8o" ], InvalidHost )
    , ( "an IPv6 literal that is not hex", [ "GET / HTTP/1.1", "Host: [zz::1]" ], InvalidHost )
    , ( "a broken percent-encoding in a Host", [ "GET / HTTP/1.1", "Host: %zz" ], InvalidHost )
    , ( "an absolute-form authority and a Host that disagree", [ "GET http://a.example/ HTTP/1.1", "Host: b.example" ], HostMismatch )
    , ( "an absolute-form target with an empty host", [ "GET http:///path HTTP/1.1", "Host: x" ], InvalidHost )
    , ( "an absolute-form target with userinfo", [ "GET http://u@h/ HTTP/1.1", "Host: h" ], InvalidHost )
    , ( "an absolute-form target with a port past 65535", [ "GET http://h:99999/ HTTP/1.1", "Host: h:99999" ], InvalidHost )
    ]


suite : Test
suite =
    describe "Http.Head"
        [ test "a plain GET: no body, the connection persists" <|
            \_ ->
                parsed [ "GET /health HTTP/1.1", "Host: x" ]
                    |> Result.map (\h -> ( ( h.method, h.target, h.version ), ( h.framing, h.persistence, h.expectsContinue ) ))
                    |> Expect.equal (Ok ( ( Request.Get, "/health", Http11 ), ( NoBody, KeepAlive, False ) ))
        , describe "refused" <|
            List.map
                (\( name, lines, expected ) -> test name <| \_ -> Expect.equal (Just expected) (problem lines))
                refusals
        , test "Content-Length: digits, repeated values that agree, and zero as no body" <|
            \_ ->
                List.map framing
                    [ [ "POST / HTTP/1.1", "Host: x", "Content-Length: 10" ]
                    , [ "POST / HTTP/1.1", "Host: x", "Content-Length: 10, 10" ]
                    , [ "POST / HTTP/1.1", "Host: x", "Content-Length: 10", "Content-Length: 10" ]
                    , [ "POST / HTTP/1.1", "Host: x", "Content-Length: 0" ]
                    ]
                    |> Expect.equal [ Ok (Length 10), Ok (Length 10), Ok (Length 10), Ok NoBody ]
        , test "Transfer-Encoding: chunked, in any case" <|
            \_ ->
                List.map framing
                    [ [ "POST / HTTP/1.1", "Host: x", "Transfer-Encoding: chunked" ]
                    , [ "POST / HTTP/1.1", "Host: x", "Transfer-Encoding: Chunked" ]
                    ]
                    |> Expect.equal [ Ok Chunked, Ok Chunked ]
        , test "persistence: HTTP/1.1 unless close, HTTP/1.0 only with keep-alive" <|
            \_ ->
                List.map (parsed >> Result.map .persistence)
                    [ [ "GET / HTTP/1.1", "Host: x", "Connection: Keep-Alive, Close" ]
                    , [ "GET / HTTP/1.0" ]
                    , [ "GET / HTTP/1.0", "Connection: keep-alive" ]
                    ]
                    |> Expect.equal [ Ok Close, Ok Close, Ok KeepAlive ]
        , test "100-continue in any case; ignored in HTTP/1.0 and for a request without a body" <|
            \_ ->
                List.map (parsed >> Result.map .expectsContinue)
                    [ [ "PUT / HTTP/1.1", "Host: x", "Content-Length: 10", "Expect: 100-Continue" ]
                    , [ "PUT / HTTP/1.0", "Content-Length: 10", "Expect: 100-continue" ]
                    , [ "PUT / HTTP/1.1", "Host: x", "Expect: 100-continue" ]
                    ]
                    |> Expect.equal [ Ok True, Ok False, Ok False ]
        , test "hosts that are valid: names, ports, IPv4, IPv6, punycode, and an empty Host" <|
            \_ ->
                List.map (\h -> parsed [ "GET / HTTP/1.1", "Host:" ++ h ] |> Result.map .authority)
                    [ " example.com:8080", " [::1]:3000", " 127.0.0.1", "", " xn--caf-dma.example", " A.Example" ]
                    |> Expect.equal (List.map Ok [ "example.com:8080", "[::1]:3000", "127.0.0.1", "", "xn--caf-dma.example", "A.Example" ])
        , test "the authority: Host's for origin-form, the target's for absolute-form, compared as hosts are" <|
            \_ ->
                List.map (parsed >> Result.map (\h -> ( h.authority, h.target )))
                    [ [ "GET /x HTTP/1.1", "Host: a.example" ]
                    , [ "GET http://A.example:80/x HTTP/1.1", "Host: a.example" ]
                    , [ "GET https://a.example/x HTTP/1.1", "Host: a.example:443" ]
                    , [ "GET http://a.example/x HTTP/1.0" ]
                    ]
                    |> Expect.equal (List.map Ok [ ( "a.example", "/x" ), ( "A.example:80", "/x" ), ( "a.example", "/x" ), ( "a.example", "/x" ) ])
        , test "leading zeros in Content-Length are not significant" <|
            \_ ->
                List.map framing
                    [ [ "POST / HTTP/1.1", "Host: x", "Content-Length: 000000000000000005" ]
                    , [ "POST / HTTP/1.1", "Host: x", "Content-Length: 0000" ]
                    ]
                    |> Expect.equal [ Ok (Length 5), Ok NoBody ]
        , test "a higher minor version of 1 is read as 1.1; Host is optional in 1.0" <|
            \_ ->
                List.map (parsed >> Result.map .version)
                    [ [ "GET / HTTP/1.2", "Host: x" ], [ "GET / HTTP/1.0" ] ]
                    |> Expect.equal [ Ok Http11, Ok Http10 ]
        , test "absolute-form is reduced to its path; the asterisk stands" <|
            \_ ->
                List.map (parsed >> Result.map .target)
                    [ [ "GET http://x/blobs/1?q=2 HTTP/1.1", "Host: x" ]
                    , [ "GET http://x?q HTTP/1.1", "Host: x" ]
                    , [ "GET https://x HTTP/1.1", "Host: x" ]
                    , [ "OPTIONS * HTTP/1.1", "Host: x" ]
                    ]
                    |> Expect.equal [ Ok "/blobs/1?q=2", Ok "/?q", Ok "/", Ok "*" ]
        , test "field names are lowercased; values lose surrounding whitespace and keep obs-text" <|
            \_ ->
                parsed [ "GET / HTTP/1.1", "Host:x", "X-Name: \t café \t" ]
                    |> Result.map .fields
                    |> Expect.equal (Ok [ ( "host", "x" ), ( "x-name", "café" ) ])
        , test "the limits: target, field count, total bytes" <|
            \_ ->
                [ problem [ "GET /" ++ String.repeat 8200 "a" ++ " HTTP/1.1", "Host: x" ]
                , problem ("GET / HTTP/1.1" :: List.map (\i -> "X-" ++ String.fromInt i ++ ": v") (List.range 1 101))
                , problem [ "GET / HTTP/1.1", "Host: x", "X-Pad: " ++ String.repeat 16400 "a" ]
                ]
                    |> Expect.equal [ Just TargetTooLong, Just HeadTooLarge, Just HeadTooLarge ]
        , test "exactly at each limit is accepted" <|
            \_ ->
                let
                    line =
                        "GET /" ++ String.repeat 8191 "a" ++ " HTTP/1.1"

                    hundred =
                        "GET / HTTP/1.1" :: "Host: x" :: List.map (\i -> "X-" ++ String.fromInt i ++ ": v") (List.range 1 99)

                    padded =
                        let
                            base =
                                head [ "GET / HTTP/1.1", "Host: x", "X-Pad: " ]
                        in
                        [ "GET / HTTP/1.1", "Host: x", "X-Pad: " ++ String.repeat (16384 - String.length base) "a" ]
                in
                [ String.length line - String.length "GET  HTTP/1.1", List.length hundred - 1, String.length (head padded) ]
                    ++ List.map (problem >> Maybe.map (always 1) >> Maybe.withDefault 0) [ [ line, "Host: x" ], hundred, padded ]
                    |> Expect.equal [ 8192, 100, 16384, 0, 0, 0 ]
        , test "a head whose lines end in a bare LF is refused" <|
            \_ ->
                Head.parse limits "GET / HTTP/1.1\nHost: x\u{000D}\n\u{000D}\n"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , test "a head without its empty line is refused" <|
            \_ ->
                Head.parse limits "GET / HTTP/1.1\u{000D}\nHost: x\u{000D}\n"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        , fuzz wellFormed "any well-formed head parses back to what was written" <|
            \( method, target, fields ) ->
                parsed ((Request.methodToString method ++ " " ++ target ++ " HTTP/1.1") :: List.map (\( n, v ) -> n ++ ": " ++ v) (( "Host", "x" ) :: fields))
                    |> Result.map (\h -> ( h.method, h.target, h.fields ))
                    |> Expect.equal (Ok ( method, target, List.map (\( n, v ) -> ( String.toLower n, v )) (( "Host", "x" ) :: fields) ))
        ]


{-| Methods, origin-form targets, and fields whose names and values cannot
collide with the framing, host or expectation rules.
-}
wellFormed : Fuzzer ( Request.Method, String, List ( String, String ) )
wellFormed =
    let
        from chars =
            Fuzz.map String.fromList (Fuzz.listOfLengthBetween 1 12 (Fuzz.oneOfValues chars))

        name =
            Fuzz.map (\s -> "X-" ++ s) (from (String.toList "abcdefghijKLMNOP0123456789-_!#$%&'*+.^`|~"))

        value =
            Fuzz.map (\s -> "v" ++ s ++ "v") (from (String.toList "abc XYZ 019 \t;,=\"()/:<>?@[]{}éÿ"))

        target =
            Fuzz.map (\s -> "/" ++ s) (from (String.toList "abcxyz019/-._~%!$&'()*+,;=:@?"))
    in
    Fuzz.map3 (\m t fs -> ( m, t, fs ))
        (Fuzz.oneOfValues (List.filter ((/=) Request.Connect) Request.allMethods))
        target
        (Fuzz.listOfLengthBetween 0 8 (Fuzz.pair name value))
