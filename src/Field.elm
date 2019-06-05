module Field
    exposing
        ( fieldToDate
        , Field(..)
        )

{-| Setting a date field on a date.

@docs fieldToDate
@docs fieldToDateClamp
@docs Field

Copyright (c) 2016-2018 Robin Luiten

-}

-- import Date exposing (Date, Day, Month)
-- import Date.Extra.Core as Core
import Derberos.Date.Delta as Delta
import Derberos.Date.Utils as Utils
import Time exposing (Zone, Posix)


{-| Configured Field and Value to set on date.

All field values are applied Modulus there maximum value.

  - DayOfWeek
      - The week keeps the same start of week day as passed in and changes day.
  - Month
      - Will not change year only the month of year.

-}
type Field
    = DayOfMonth Int
    | Year Int


{-| Set a field on a date to a specific value.

If your value in field is out side of valid range for
the date field this function will return Nothing.

  - DayOfWeek cannot be invalid input range
  - Month cannot be invalid

Valid ranges

  - Millisecond 0 to 999
  - Second 0 to 59
  - Minute 0 to 59
  - Hour 0 to 23
  - DayOfMonth 1 to max day of month for year
  - Year >= 0

-}
fieldToDate : Field -> Posix -> Maybe Posix
fieldToDate field date =
    case field of
        DayOfMonth day ->
            let
                year =
                    Time.toYear Time.utc date
                month =
                    Time.toMonth Time.utc date

                maxDays =
                    Utils.numberOfDaysInMonth year month
            in
                if day < 1 || day > maxDays then
                    Nothing
                else
                    Just <| Delta.addDays (day - (Time.toDay Time.utc date)) date

        Year year ->
            if year < 0 then
                Nothing
            else
                Just <| Delta.addYears (year - (Time.toYear Time.utc  date)) date
