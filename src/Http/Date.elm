module Http.Date exposing (Date, fromMillis, toString)

{-| The `Date` header's value, IMF-fixdate (RFC 9110 section 5.6.7):

    Sun, 06 Nov 1994 08:49:37 GMT

The format has one-second resolution; `Time`'s accessors floor to the
second. The wall clock is the host's, read by the bridge when it reported
the event being answered; it is never used for a deadline, because it can
jump.

-}

import Time exposing (Month(..), Weekday(..))


type Date
    = Date String


fromMillis : Int -> Date
fromMillis wallMs =
    let
        t =
            Time.millisToPosix wallMs

        two n =
            String.padLeft 2 '0' (String.fromInt n)
    in
    Date
        (weekday (Time.toWeekday Time.utc t)
            ++ ", "
            ++ two (Time.toDay Time.utc t)
            ++ " "
            ++ month (Time.toMonth Time.utc t)
            ++ " "
            ++ String.padLeft 4 '0' (String.fromInt (Time.toYear Time.utc t))
            ++ " "
            ++ two (Time.toHour Time.utc t)
            ++ ":"
            ++ two (Time.toMinute Time.utc t)
            ++ ":"
            ++ two (Time.toSecond Time.utc t)
            ++ " GMT"
        )


toString : Date -> String
toString (Date text) =
    text


weekday : Weekday -> String
weekday day =
    case day of
        Mon ->
            "Mon"

        Tue ->
            "Tue"

        Wed ->
            "Wed"

        Thu ->
            "Thu"

        Fri ->
            "Fri"

        Sat ->
            "Sat"

        Sun ->
            "Sun"


month : Month -> String
month m =
    case m of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"
