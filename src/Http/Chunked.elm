module Http.Chunked exposing (Limits, Outcome(..), decode)

{-| The chunked transfer coding (RFC 9112 section 7.1): the reference
semantics the bridge's decoder must match byte for byte (see
`tests/chunked.js`).

The bridge decodes chunked bodies itself, at stream speed, because body
bytes never cross into Elm. What it may do with them is defined here, over
a whole input with one character per byte, so the definition can be read
rather than run:

    chunked-body = *chunk last-chunk trailer-section CRLF
    chunk        = chunk-size [ BWS ";" ext ] CRLF chunk-data CRLF
    chunk-size   = 1*13HEXDIG
    last-chunk   = 1*13("0") [ BWS ";" ext ] CRLF

Extensions and trailer fields are read only to find where they end: a
control character other than HTAB, a bare CR or LF, or whitespace after a
size that no `;` follows, is malformed. `maxLine` bounds a size line's bytes
up to its CR; `maxTrailer` bounds the trailer section, every byte from the
end of the last chunk's line to the final LF.

-}


type alias Limits =
    { maxLine : Int
    , maxTrailer : Int
    }


{-| `Complete` carries the body's data and how many input bytes its
framing took; what follows is the next request's. `Incomplete` means the
input ended first. `Malformed` names the offset of the byte that broke the
framing, or crossed a limit.
-}
type Outcome
    = Complete { data : String, used : Int }
    | Incomplete
    | Malformed Int


type State
    = Size
    | Bws
    | Ext
    | SizeLF
    | Data Int
    | DataCR
    | DataLF
    | Trailer
    | TrailerLine
    | TrailerLF
    | EndLF
    | Done


type alias Machine =
    { state : State
    , size : Int
    , digits : Int
    , line : Int
    , trailer : Int
    }


decode : Limits -> String -> Outcome
decode limits input =
    run limits { state = Size, size = 0, digits = 0, line = 0, trailer = 0 } (String.toList input) 0 []


run : Limits -> Machine -> List Char -> Int -> List Char -> Outcome
run limits m chars offset data =
    case ( m.state, chars ) of
        ( Done, _ ) ->
            Complete { data = String.fromList (List.reverse data), used = offset }

        ( _, [] ) ->
            Incomplete

        ( Data remaining, c :: rest ) ->
            run limits
                { m
                    | state =
                        if remaining == 1 then
                            DataCR

                        else
                            Data (remaining - 1)
                }
                rest
                (offset + 1)
                (c :: data)

        ( _, c :: rest ) ->
            let
                b =
                    Char.toCode c

                inLine =
                    (m.state == Size || m.state == Bws || m.state == Ext) && b /= 0x0D

                inTrailer =
                    m.state == Trailer || m.state == TrailerLine || m.state == TrailerLF || m.state == EndLF
            in
            case transition m b of
                Nothing ->
                    Malformed offset

                Just next ->
                    let
                        counted =
                            { next
                                | line =
                                    if inLine then
                                        next.line + 1

                                    else
                                        next.line
                                , trailer =
                                    if inTrailer then
                                        next.trailer + 1

                                    else
                                        next.trailer
                            }
                    in
                    if counted.line > limits.maxLine || counted.trailer > limits.maxTrailer then
                        Malformed offset

                    else
                        run limits counted rest (offset + 1) data


{-| One byte outside chunk data.
-}
transition : Machine -> Int -> Maybe Machine
transition m b =
    case m.state of
        Size ->
            if isHex b then
                if m.digits >= 13 then
                    Nothing

                else
                    Just { m | digits = m.digits + 1, size = m.size * 16 + hexValue b }

            else if m.digits == 0 then
                Nothing

            else if b == 0x3B then
                Just { m | state = Ext }

            else if b == 0x20 || b == 0x09 then
                Just { m | state = Bws }

            else if b == 0x0D then
                Just { m | state = SizeLF }

            else
                Nothing

        Bws ->
            if b == 0x3B then
                Just { m | state = Ext }

            else if b == 0x20 || b == 0x09 then
                Just m

            else
                Nothing

        Ext ->
            if b == 0x0D then
                Just { m | state = SizeLF }

            else if isLineByte b then
                Just m

            else
                Nothing

        SizeLF ->
            if b /= 0x0A then
                Nothing

            else
                Just
                    { m
                        | state =
                            if m.size == 0 then
                                Trailer

                            else
                                Data m.size
                        , size = 0
                        , digits = 0
                        , line = 0
                    }

        DataCR ->
            if b == 0x0D then
                Just { m | state = DataLF }

            else
                Nothing

        DataLF ->
            if b == 0x0A then
                Just { m | state = Size }

            else
                Nothing

        Trailer ->
            if b == 0x0D then
                Just { m | state = EndLF }

            else if isLineByte b then
                Just { m | state = TrailerLine }

            else
                Nothing

        TrailerLine ->
            if b == 0x0D then
                Just { m | state = TrailerLF }

            else if isLineByte b then
                Just m

            else
                Nothing

        TrailerLF ->
            if b == 0x0A then
                Just { m | state = Trailer }

            else
                Nothing

        EndLF ->
            if b == 0x0A then
                Just { m | state = Done }

            else
                Nothing

        Data _ ->
            Nothing

        Done ->
            Nothing


isHex : Int -> Bool
isHex b =
    (b >= 0x30 && b <= 0x39) || (b >= 0x41 && b <= 0x46) || (b >= 0x61 && b <= 0x66)


hexValue : Int -> Int
hexValue b =
    if b <= 0x39 then
        b - 0x30

    else if b <= 0x46 then
        b - 0x41 + 10

    else
        b - 0x61 + 10


isLineByte : Int -> Bool
isLineByte b =
    b == 0x09 || (b >= 0x20 && b /= 0x7F)
