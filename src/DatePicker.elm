module DatePicker exposing (init, Model, Msg, update, view, selectedDate)

{-|

@docs init, Model, Msg, update, view, selectedDate

-}

-- import Date.Extra.Core exposing (daysInMonth, intToMonth, isoDayOfWeek, toFirstOfMonth)
-- import Date.Extra.Duration as Duration
-- import Date.Extra.Field as Field
-- import Date.Extra.Format as DateFormat

import Derberos.Date.Calendar as Calendar
import Derberos.Date.Delta as Delta
import Derberos.Date.Utils as Utils
import Field
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes
import Time exposing (Posix, Zone)
import Timestamp


{-| -}
type alias Model =
    { zone : Zone
    , date : Posix
    , selectingYear : Bool
    , mainColor : String
    }


{-| Pass date picker settings and get initial time picker model
-}
init : Zone -> Posix -> String -> Model
init zone date mainColor =
    { zone = zone
    , date = date
    , selectingYear = False
    , mainColor = mainColor
    }


{-| Returns currentyl selected date
-}
selectedDate : Model -> Posix
selectedDate model =
    model.date


formattedDay : Time.Zone -> Time.Posix -> String
formattedDay zone time =
    Timestamp.formattedDay zone time


formattedMonth : Time.Zone -> Time.Posix -> String
formattedMonth zone time =
    Timestamp.formattedMonth zone time


{-| -}
type Msg
    = Noop
    | YearSelection
    | DaySelection
    | PrevMonth
    | NextMonth
    | SelectYear Int
    | SelectDay Int


{-| -}
update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        YearSelection ->
            { model | selectingYear = True }

        DaySelection ->
            { model | selectingYear = False }

        PrevMonth ->
            { model | date = Delta.addMonths -1 model.zone model.date }

        NextMonth ->
            { model | date = Delta.addMonths 1 model.zone model.date }

        SelectYear year ->
            case Field.fieldToDate (Field.Year year) model.date of
                Just date ->
                    { model
                        | date = date
                        , selectingYear = False
                    }

                Nothing ->
                    model

        SelectDay day ->
            case Field.fieldToDate (Field.DayOfMonth day) model.date of
                Just date ->
                    { model | date = date }

                Nothing ->
                    model


{-| -}
view : Model -> Html Msg
view model =
    let
        content =
            if model.selectingYear then
                yearPicker model

            else
                picker model
    in
    div [ Html.Attributes.class "date-picker" ]
        [ header model
        , content
        ]


header : Model -> Html Msg
header model =
    let
        ( dayClass, yearClass ) =
            if model.selectingYear then
                ( "day", "year selected" )

            else
                ( "day selected", "year" )
    in
    div
        [ Html.Attributes.class "header"
        , Html.Attributes.style "background-color" model.mainColor
        ]
        [ div [ Html.Attributes.class yearClass, onClick YearSelection ]
            [ Html.text <| String.fromInt <| Time.toYear model.zone model.date
            ]
        , div [ Html.Attributes.class dayClass, onClick DaySelection ]
            [ Html.text <| formattedDay model.zone model.date
            ]
        ]


weekDays : Html Msg
weekDays =
    let
        days =
            List.map (\day -> span [] [ Html.text day ]) [ "M", "T", "W", "T", "F", "S", "S" ]
    in
    div [ Html.Attributes.class "week-days" ]
        days


monthDays : Model -> Html Msg
monthDays model =
    let
        year =
            Time.toYear model.zone model.date

        month =
            Time.toMonth model.zone model.date

        daysCount =
            Utils.numberOfDaysInMonth year month

        weekDay =
            (+) 1 <| Utils.weekdayToNumber <| Utils.getWeekday model.zone <| Calendar.getFirstDayOfMonth model.zone <| model.date

        leftPadding =
            weekDay - 1

        rightPadding =
            modBy 7 (7 - modBy 7 (daysCount + leftPadding))

        weeks =
            chunks 7 (List.repeat leftPadding 0 ++ List.range 1 daysCount ++ List.repeat rightPadding 0)

        rows =
            List.map (\week -> weekRow week (Time.toDay model.zone model.date) model.mainColor) weeks
    in
    div [ Html.Attributes.class "month-days" ]
        [ div [ Html.Attributes.class "day-rows" ]
            rows
        ]


weekRow : List Int -> Int -> String -> Html Msg
weekRow days currentDay mainColor =
    div [ Html.Attributes.class "days-row" ]
        (List.map (\day -> dayCell day currentDay mainColor) days)


dayCell : Int -> Int -> String -> Html Msg
dayCell dayNumber currentDay mainColor =
    if dayNumber > 0 then
        let
            backgroundClass =
                if dayNumber == currentDay then
                    "day-background selected"

                else
                    "day-background"
        in
        button
            [ Html.Attributes.class "day", onClick <| SelectDay dayNumber ]
            [ div
                [ Html.Attributes.class backgroundClass
                , Html.Attributes.style "background-color" mainColor
                ]
                []
            , span [ Html.Attributes.class "day-number" ] [ Html.text (String.fromInt dayNumber) ]
            ]

    else
        div [ Html.Attributes.class "empty-day" ] []


picker : Model -> Html Msg
picker model =
    div [ Html.Attributes.class "picker" ]
        [ div [ Html.Attributes.class "month-year-selector" ]
            [ button [ Html.Attributes.class "navigation-wrapper", onClick PrevMonth ]
                [ Svg.svg
                    [ Svg.Attributes.class "navigation-icon"
                    , Svg.Attributes.viewBox "0 0 24 24"
                    ]
                    [ Svg.path [ Svg.Attributes.d "M15.41 7.41L14 6l-6 6 6 6 1.41-1.41L10.83 12z" ] []
                    ]
                ]
            , div [ Html.Attributes.class "month-year" ]
                [ Html.text <| formattedMonth model.zone model.date
                ]
            , button [ Html.Attributes.class "navigation-wrapper", onClick NextMonth ]
                [ Svg.svg
                    [ Svg.Attributes.class "navigation-icon"
                    , Svg.Attributes.viewBox "0 0 24 24"
                    ]
                    [ Svg.path [ Svg.Attributes.d "M10 6L8.59 7.41 13.17 12l-4.58 4.59L10 18l6-6z" ] []
                    ]
                ]
            ]
        , weekDays
        , monthDays model
        ]


yearPicker : Model -> Html Msg
yearPicker model =
    let
        yearButtons =
            List.map (\y -> yearButton y (Time.toYear model.zone model.date)) <| List.range 1917 2117
    in
    div [ Html.Attributes.class "year-picker" ]
        [ div [ Html.Attributes.class "year-list-wrapper" ]
            [ div [ Html.Attributes.class "year-list" ]
                yearButtons
            ]
        ]


yearButton : Int -> Int -> Html Msg
yearButton year currentYear =
    let
        spanClass =
            if year == currentYear then
                "selected"

            else
                ""
    in
    button [ Html.Attributes.class "year", onClick <| SelectYear year ]
        [ span [ Html.Attributes.class spanClass ]
            [ text "yearButton" ]
        ]


chunks : Int -> List a -> List (List a)
chunks k xs =
    if List.length xs > k then
        List.take k xs :: chunks k (List.drop k xs)

    else
        [ xs ]
