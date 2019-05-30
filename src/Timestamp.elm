module Timestamp exposing (format)

import Time exposing (Month(..), Weekday(..))


type WordLength
    = Short
    | Long


formatMonth : Time.Zone -> Time.Posix -> WordLength -> String
formatMonth zone time wordLength =
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
    case wordLength of
        Long ->
            longMonth

        Short ->
            shortMonth


formatDayOfWeek : Time.Zone -> Time.Posix -> WordLength -> String
formatDayOfWeek zone time wordLength =
    let
        longDayOfWeek =
            case Time.toWeekday zone time of
                Mon ->
                    "Monday"

                Tue ->
                    "Tuesday"

                Wed ->
                    "Wednesday"

                Thu ->
                    "Thursday"

                Fri ->
                    "Friday"

                Sat ->
                    "Saturday"

                Sun ->
                    "Sunday"

        shortDayOfWeek =
            String.left 3 longDayOfWeek
    in
    case wordLength of
        Long ->
            longDayOfWeek

        Short ->
            shortDayOfWeek


format : Time.Zone -> Time.Posix -> String
format zone time =
    let
        month =
            formatMonth zone time Long

        dayOfWeek =
            formatDayOfWeek zone time Short

        day =
            String.fromInt (Time.toDay zone time)

        year =
            String.fromInt (Time.toYear zone time)
    in
    dayOfWeek ++ " " ++ month ++ " " ++ day ++ ", " ++ year
