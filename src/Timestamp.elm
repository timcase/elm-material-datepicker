module Timestamp exposing (formattedDay, formattedMonth)

import Time exposing (Month(..), Weekday(..))


type Abbreviation
    = Abbreviated
    | NotAbbreviated


formatMonth : Time.Zone -> Time.Posix -> Abbreviation -> String
formatMonth zone time abbreviation =
    let
        longMonth =
            case Time.toMonth zone time of
                Jan ->
                    "January"

                Feb ->
                    "February"

                Mar ->
                    "March"

                Apr ->
                    "April"

                May ->
                    "May"

                Jun ->
                    "June"

                Jul ->
                    "July"

                Aug ->
                    "August"

                Sep ->
                    "September"

                Oct ->
                    "October"

                Nov ->
                    "November"

                Dec ->
                    "December"

        shortMonth =
            String.left 3 longMonth
    in
    case abbreviation of
        NotAbbreviated ->
            longMonth

        Abbreviated ->
            shortMonth


formatDayOfWeek : Time.Zone -> Time.Posix -> String
formatDayOfWeek zone time =
    case Time.toWeekday zone time of
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


formattedMonth : Time.Zone -> Time.Posix -> String
formattedMonth zone time =
    let
        month =
            formatMonth zone time NotAbbreviated

        dayOfWeek =
            formatDayOfWeek zone time

        day =
            String.fromInt (Time.toDay zone time)

        year =
            String.fromInt (Time.toYear zone time)
    in
    month ++ " " ++ year


formattedDay : Time.Zone -> Time.Posix -> String
formattedDay zone time =
    let
        month =
            formatMonth zone time Abbreviated

        dayOfWeek =
            formatDayOfWeek zone time

        day =
            String.fromInt (Time.toDay zone time)
    in
    dayOfWeek ++ ", " ++ month ++ " " ++ day
