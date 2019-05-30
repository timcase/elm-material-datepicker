module Timestamp exposing (format)

import Time exposing (Month(..))


type MonthAbbrLength
    = Short
    | Long


formatMonth : Time.Zone -> Time.Posix -> MonthAbbrLength -> String
formatMonth zone time monthAbbrLength =
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
    case monthAbbrLength of
        Long ->
            longMonth

        Short ->
            shortMonth


format : Time.Zone -> Time.Posix -> String
format zone time =
    let
        month =
            formatMonth zone time Long

        day =
            String.fromInt (Time.toDay zone time)

        year =
            String.fromInt (Time.toYear zone time)
    in
    month ++ " " ++ day ++ ", " ++ year
