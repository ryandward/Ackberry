module Http.Head exposing
    ( Framing(..)
    , Head
    , Limits
    , Persistence(..)
    , Problem(..)
    , Version(..)
    , parse
    )

{-| A request head, parsed from the bytes the client sent (RFC 9112).

The bridge only delimits a head: it finds the empty line that ends it,
within Elm's byte limit, and hands it over as text with one character per
byte (latin1), the final CRLF CRLF included. Every rule about what a head
means lives here: the request line, the field lines, which host it names,
how the body is framed, whether the connection persists, and what the
client expects.

Ambiguity is refused, never resolved. A body whose length can be read two
ways (Content-Length beside Transfer-Encoding, two different
Content-Lengths) is a 400, and so is a request that names two hosts: two
readers that disagree about a request disagree about everything after it.

Problems and the status each one calls for:

    MalformedRequestLine     400   includes a head that does not end in CRLF CRLF
    TargetTooLong            414
    UnsupportedVersion       505   a major version other than 1
    UnsupportedMethod        501   unknown, longer than any implemented, or CONNECT
    HeadTooLarge             431   too many bytes or too many fields
    MalformedField           400   obs-fold, space before the colon, a control character
    MissingHost              400   HTTP/1.1 without Host
    MultipleHosts            400
    InvalidHost              400   a Host or an absolute-form authority that is not
                                   uri-host [ ":" port ], has userinfo, or is empty
    HostMismatch             400   an absolute-form authority and a Host that disagree
    InvalidContentLength     400   not digits, or values that disagree
    ContentLengthTooLarge    413   more significant digits than an Int holds exactly
    AmbiguousLength          400   Content-Length with Transfer-Encoding
    FaultyFraming            400   chunked not last (a lone gzip included), chunked
                                   twice, or any Transfer-Encoding in HTTP/1.0
    UnsupportedCoding        501   a transfer coding other than chunked before it
    UnsupportedExpectation   417

-}

import Http.Request as Request exposing (Method)


type Version
    = Http10
    | Http11


type Framing
    = NoBody
    | Length Int
    | Chunked


type Persistence
    = KeepAlive
    | Close


{-| `target` is origin-form (path and query) or `*`. `authority` is the
host the request names: an absolute-form target's (RFC 9112 section 3.2.2
says it overrides Host), otherwise Host's, empty if there is none. `fields`
keeps every field line in order, names lowercased and values stripped of
surrounding whitespace. `expectsContinue` is never true for a request
without a body.
-}
type alias Head =
    { method : Method
    , target : String
    , authority : String
    , version : Version
    , fields : List ( String, String )
    , framing : Framing
    , persistence : Persistence
    , expectsContinue : Bool
    }


type alias Limits =
    { maxHeadBytes : Int
    , maxTarget : Int
    , maxFields : Int
    }


type Problem
    = MalformedRequestLine
    | TargetTooLong
    | UnsupportedVersion
    | UnsupportedMethod String
    | HeadTooLarge
    | MalformedField
    | MissingHost
    | MultipleHosts
    | InvalidHost
    | HostMismatch
    | InvalidContentLength
    | ContentLengthTooLarge
    | AmbiguousLength
    | FaultyFraming
    | UnsupportedCoding
    | UnsupportedExpectation


parse : Limits -> String -> Result Problem Head
parse limits raw =
    if String.length raw > limits.maxHeadBytes then
        Err HeadTooLarge

    else if not (String.endsWith "\u{000D}\n\u{000D}\n" raw) then
        Err MalformedRequestLine

    else
        case String.split "\u{000D}\n" (String.dropRight 4 raw) of
            [] ->
                Err MalformedRequestLine

            line :: fieldLines ->
                if List.length fieldLines > limits.maxFields then
                    Err HeadTooLarge

                else
                    Result.map2 Tuple.pair (requestLine limits line) (allFields fieldLines)
                        |> Result.andThen interpret


type alias RequestLine =
    { method : Method
    , target : Target
    , version : Version
    }


{-| An absolute-form target keeps its scheme, so that its authority can be
compared with Host on the same terms.
-}
type Target
    = Origin String
    | Asterisk
    | Absolute { scheme : String, authority : String, rest : String }


interpret : ( RequestLine, List ( String, String ) ) -> Result Problem Head
interpret ( line, fields ) =
    Result.map3
        (\authority framing expects ->
            { method = line.method
            , target =
                case line.target of
                    Origin path ->
                        path

                    Asterisk ->
                        "*"

                    Absolute absolute ->
                        absolute.rest
            , authority = authority
            , version = line.version
            , fields = fields
            , framing = framing
            , persistence = persistence line.version fields
            , expectsContinue = expects && framing /= NoBody
            }
        )
        (authorityOf line fields)
        (framingOf line.version fields)
        (expectation line.version fields)



-- THE REQUEST LINE


{-| Each part is judged on its own terms, so the status names the part at
fault: an endless method is 501, an endless target 414.
-}
requestLine : Limits -> String -> Result Problem RequestLine
requestLine limits line =
    case String.split " " line of
        [ method, target, version ] ->
            methodOf method
                |> Result.andThen
                    (\m ->
                        Result.map2 (RequestLine m)
                            (targetOf limits m target)
                            (versionOf version)
                    )

        _ ->
            Err MalformedRequestLine


{-| Methods are case-sensitive, so `get` is a token nobody implements. A
token longer than any method could be is not echoed back whole. CONNECT is
a method this server does not implement: it tunnels, and nothing here does.
-}
methodOf : String -> Result Problem Method
methodOf token =
    if not (isToken token) then
        Err MalformedRequestLine

    else
        case List.filter (\m -> Request.methodToString m == token) Request.allMethods of
            [ Request.Connect ] ->
                Err (UnsupportedMethod token)

            m :: _ ->
                Ok m

            [] ->
                Err (UnsupportedMethod (String.left 32 token))


{-| Origin-form; the asterisk, for OPTIONS only (RFC 9112 section 3.2.4);
or absolute-form, which a server must accept (section 3.2.2). A fragment is
never part of a request target.
-}
targetOf : Limits -> Method -> String -> Result Problem Target
targetOf limits method target =
    if String.length target > limits.maxTarget then
        Err TargetTooLong

    else if String.isEmpty target || String.contains "#" target || not (String.all isVisible target) then
        Err MalformedRequestLine

    else if target == "*" then
        if method == Request.Options then
            Ok Asterisk

        else
            Err MalformedRequestLine

    else if String.startsWith "/" target then
        Ok (Origin target)

    else
        case List.filter (\scheme -> String.startsWith (scheme ++ "://") (String.toLower target)) [ "http", "https" ] of
            scheme :: _ ->
                absoluteForm scheme (String.dropLeft (String.length scheme + 3) target)

            [] ->
                Err MalformedRequestLine


{-| `afterScheme` is everything after `scheme://`. The authority ends at the
first `/` or `?`. An `http` or `https` URI with an empty host or with
userinfo is refused (RFC 9110 sections 4.2.1 and 4.2.4); userinfo needs no
rule of its own, because `@` is never part of `uri-host [ ":" port ]`.
-}
absoluteForm : String -> String -> Result Problem Target
absoluteForm scheme afterScheme =
    let
        end =
            String.indexes "/" afterScheme ++ String.indexes "?" afterScheme |> List.minimum |> Maybe.withDefault (String.length afterScheme)

        authority =
            String.left end afterScheme

        rest =
            String.dropLeft end afterScheme
    in
    if not (validAuthority authority) || String.isEmpty (hostPart authority) then
        Err InvalidHost

    else
        Ok
            (Absolute
                { scheme = scheme
                , authority = authority
                , rest =
                    if String.isEmpty rest then
                        "/"

                    else if String.startsWith "?" rest then
                        "/" ++ rest

                    else
                        rest
                }
            )


{-| `HTTP/` DIGIT `.` DIGIT, case-sensitive. A higher minor version of 1 is
read as 1.1 (RFC 9110 section 2.5); any other major version is refused.
-}
versionOf : String -> Result Problem Version
versionOf version =
    case String.toList version of
        [ 'H', 'T', 'T', 'P', '/', major, '.', minor ] ->
            if not (Char.isDigit major && Char.isDigit minor) then
                Err MalformedRequestLine

            else if major /= '1' then
                Err UnsupportedVersion

            else if minor == '0' then
                Ok Http10

            else
                Ok Http11

        _ ->
            Err MalformedRequestLine



-- FIELD LINES


allFields : List String -> Result Problem (List ( String, String ))
allFields lines =
    List.foldr (\line acc -> Result.map2 (::) (fieldOf line) acc) (Ok []) lines


{-| `name ":" OWS value OWS`. The name must be a token, which also refuses
obs-fold (a continuation line starts with whitespace, so its name is not a
token) and whitespace before the colon (RFC 9112 section 5.1). CR, LF, NUL
and the other controls are never part of a value.
-}
fieldOf : String -> Result Problem ( String, String )
fieldOf line =
    case String.indexes ":" line of
        colon :: _ ->
            let
                name =
                    String.left colon line

                value =
                    trimOws (String.dropLeft (colon + 1) line)
            in
            if isToken name && String.all isFieldChar value then
                Ok ( String.toLower name, value )

            else
                Err MalformedField

        [] ->
            Err MalformedField


{-| Not `String.trim`, which strips JavaScript's whitespace, U+00A0
included: in a latin1 head that is byte 0xA0, legal obs-text.
-}
trimOws : String -> String
trimOws =
    let
        ows c =
            c == ' ' || c == '\t'

        dropWhileLeft s =
            case String.uncons s of
                Just ( c, rest ) ->
                    if ows c then
                        dropWhileLeft rest

                    else
                        s

                Nothing ->
                    s
    in
    dropWhileLeft >> String.reverse >> dropWhileLeft >> String.reverse


valuesOf : String -> List ( String, String ) -> List String
valuesOf name fields =
    List.filterMap
        (\( n, v ) ->
            if n == name then
                Just v

            else
                Nothing
        )
        fields


{-| A field's comma-separated list across all of its lines, members trimmed
and lowercased, empty members dropped.
-}
membersOf : String -> List ( String, String ) -> List String
membersOf name fields =
    valuesOf name fields
        |> List.concatMap (String.split ",")
        |> List.map (trimOws >> String.toLower)
        |> List.filter (not << String.isEmpty)



-- HOSTS


{-| HTTP/1.1 needs exactly one Host and HTTP/1.0 at most one, and a Host
must be `uri-host [ ":" port ]` (RFC 9112 section 3.2). With an
absolute-form target, its authority is the one that counts, and a Host
that names another host is refused rather than ignored: the client was
required to send the same authority in both (section 3.2), and something
in front of this server may have routed on the Host.
-}
authorityOf : RequestLine -> List ( String, String ) -> Result Problem String
authorityOf line fields =
    let
        hosts =
            valuesOf "host" fields
    in
    case ( line.version, hosts ) of
        ( Http11, [] ) ->
            Err MissingHost

        ( _, _ :: _ :: _ ) ->
            Err MultipleHosts

        _ ->
            case List.filter (not << validAuthority) hosts of
                _ :: _ ->
                    Err InvalidHost

                [] ->
                    case ( line.target, hosts ) of
                        ( Absolute absolute, [ given ] ) ->
                            if sameAuthority absolute.scheme absolute.authority given then
                                Ok absolute.authority

                            else
                                Err HostMismatch

                        ( Absolute absolute, _ ) ->
                            Ok absolute.authority

                        ( _, given :: _ ) ->
                            Ok given

                        ( _, [] ) ->
                            Ok ""


{-| `uri-host [ ":" port ]` (RFC 3986): an IPv6 literal in brackets, or a
reg-name (which covers IPv4). The port is digits, at most 65535. A comma is
legal in a reg-name but is refused, because it is also exactly what two
Host lines folded into one look like, and no DNS name contains one. IPv6
literals are judged by their characters, not their full grammar; IPvFuture
and zone identifiers are refused.
-}
validAuthority : String -> Bool
validAuthority authority =
    let
        ( host, portPart ) =
            splitPort authority
    in
    validPort portPart
        && (if String.startsWith "[" host then
                String.endsWith "]" host
                    && String.contains ":" host
                    && String.all (\c -> Char.isHexDigit c || c == ':' || c == '.') (String.slice 1 -1 host)

            else
                validRegName host
           )


{-| The host and the port, which is `Nothing` when there is no colon.
-}
splitPort : String -> ( String, Maybe String )
splitPort authority =
    let
        portColon =
            if String.startsWith "[" authority then
                String.indexes "]:" authority |> List.head |> Maybe.map ((+) 1)

            else
                String.indexes ":" authority |> List.head
    in
    case portColon of
        Just i ->
            ( String.left i authority, Just (String.dropLeft (i + 1) authority) )

        Nothing ->
            ( authority, Nothing )


hostPart : String -> String
hostPart =
    splitPort >> Tuple.first


validPort : Maybe String -> Bool
validPort portPart =
    case portPart of
        Nothing ->
            True

        Just "" ->
            True

        Just digits ->
            String.length digits <= 5 && String.all Char.isDigit digits && (String.toInt digits |> Maybe.map (\n -> n <= 65535) |> Maybe.withDefault False)


validRegName : String -> Bool
validRegName name =
    case String.uncons name of
        Nothing ->
            True

        Just ( '%', rest ) ->
            case String.toList (String.left 2 rest) of
                [ a, b ] ->
                    Char.isHexDigit a && Char.isHexDigit b && validRegName (String.dropLeft 2 rest)

                _ ->
                    False

        Just ( c, rest ) ->
            (Char.isAlphaNum c || String.contains (String.fromChar c) "-._~!$&'()*+;=") && validRegName rest


{-| The same host, compared as hosts are: without case, and with the
scheme's default port the same as no port.
-}
sameAuthority : String -> String -> String -> Bool
sameAuthority scheme a b =
    let
        normal authority =
            let
                ( host, portPart ) =
                    splitPort (String.toLower authority)

                defaultPort =
                    if scheme == "https" then
                        "443"

                    else
                        "80"
            in
            case portPart of
                Just p ->
                    if p == "" || p == defaultPort then
                        host

                    else
                        host ++ ":" ++ p

                Nothing ->
                    host
    in
    normal a == normal b



-- FRAMING


{-| RFC 9112 section 6.3, with every case that lets two readers disagree
refused. Leading zeros are not significant, so `0005` is 5.
-}
framingOf : Version -> List ( String, String ) -> Result Problem Framing
framingOf version fields =
    let
        lengths =
            valuesOf "content-length" fields
                |> List.concatMap (String.split ",")
                |> List.map trimOws
    in
    if not (List.isEmpty (valuesOf "transfer-encoding" fields)) then
        if version == Http10 then
            Err FaultyFraming

        else if not (List.isEmpty lengths) then
            Err AmbiguousLength

        else
            codings (membersOf "transfer-encoding" fields)

    else
        case lengths of
            [] ->
                Ok NoBody

            first :: rest ->
                let
                    significant =
                        dropLeadingZeros first
                in
                if not (List.all (\l -> l == first) rest) || String.isEmpty first || not (String.all Char.isDigit first) then
                    Err InvalidContentLength

                else if String.length significant > 15 then
                    Err ContentLengthTooLarge

                else
                    case String.toInt significant of
                        Just 0 ->
                            Ok NoBody

                        Just n ->
                            Ok (Length n)

                        Nothing ->
                            Err InvalidContentLength


dropLeadingZeros : String -> String
dropLeadingZeros digits =
    case String.uncons digits of
        Just ( '0', rest ) ->
            if String.isEmpty rest then
                digits

            else
                dropLeadingZeros rest

        _ ->
            digits


{-| Only a single, bare `chunked`, last, is a body this server can read.
-}
codings : List String -> Result Problem Framing
codings members =
    let
        isChunked member =
            (String.split ";" member |> List.head |> Maybe.withDefault "" |> trimOws) == "chunked"
    in
    case List.reverse members of
        "chunked" :: earlier ->
            if List.any isChunked earlier then
                Err FaultyFraming

            else if List.isEmpty earlier then
                Ok Chunked

            else
                Err UnsupportedCoding

        _ ->
            Err FaultyFraming



-- PERSISTENCE AND EXPECTATIONS


persistence : Version -> List ( String, String ) -> Persistence
persistence version fields =
    let
        options =
            membersOf "connection" fields
    in
    case version of
        Http11 ->
            if List.member "close" options then
                Close

            else
                KeepAlive

        Http10 ->
            if List.member "keep-alive" options && not (List.member "close" options) then
                KeepAlive

            else
                Close


{-| A 100-continue expectation in an HTTP/1.0 request is ignored (RFC 9110
section 10.1.1); any other expectation is refused.
-}
expectation : Version -> List ( String, String ) -> Result Problem Bool
expectation version fields =
    let
        members =
            membersOf "expect" fields
    in
    if List.all (\m -> m == "100-continue") members then
        Ok (version == Http11 && not (List.isEmpty members))

    else
        Err UnsupportedExpectation



-- CHARACTERS


isToken : String -> Bool
isToken s =
    not (String.isEmpty s) && String.all isTchar s


{-| `Char.isAlphaNum` is ASCII-only.
-}
isTchar : Char -> Bool
isTchar c =
    Char.isAlphaNum c || String.contains (String.fromChar c) "!#$%&'*+-.^_`|~"


isVisible : Char -> Bool
isVisible c =
    let
        code =
            Char.toCode c
    in
    code >= 0x21 && code <= 0x7E


{-| VCHAR, obs-text, space and tab.
-}
isFieldChar : Char -> Bool
isFieldChar c =
    let
        code =
            Char.toCode c
    in
    (code >= 0x20 && code <= 0x7E) || code == 0x09 || (code >= 0x80 && code <= 0xFF)
