module Page.Alarm exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom
import Browser.Events
import Colours exposing (colorA, colorC, colorD, colorToString)
import Debounce
import Duration
import Element exposing (Element, html)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (thumb)
import Html.Events.Extra.Touch as Touch
import Http
import Json.Decode exposing (Decoder, bool, field, float, map2, map3)
import Json.Encode
import List exposing (singleton)
import Maybe exposing (map, withDefault)
import Svg exposing (Svg, circle, svg)
import Svg.Attributes exposing (cx, cy, fill, r, stroke, strokeWidth, viewBox)
import Task
import Utils exposing (scaled, urlRoot)



-- Model


type alias AlarmTime =
    { hour : Float
    , minute : Float
    }


type alias Model =
    { alarmPos : Float
    , alarmEnabled : Bool

    -- UI Component state
    , debounce : Debounce.Debounce Float
    , viewSize : ( Int, Int )
    }


init : ( Model, Cmd Msg )
init =
    ( { alarmPos = 0
      , alarmEnabled = False
      , debounce = Debounce.init
      , viewSize = ( 0, 0 )
      }
    , Cmd.batch
        [ Http.get { url = urlRoot ++ "/alarm", expect = Http.expectJson GotAlarm alarmDecoder }
        , Task.perform (\v -> ViewChanged ( round v.viewport.width, round v.viewport.height )) Browser.Dom.getViewport
        ]
    )


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = DebounceMsg
    }



-- Update


type Msg
    = MoveAt ( Float, Float )
    | GotAlarm (Result Http.Error ( AlarmTime, Bool ))
    | DebounceMsg Debounce.Msg
    | ViewChanged ( Int, Int )
    | Updated (Result Http.Error ())
    | ToggleChanged Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveAt ( x, y ) ->
            let
                alarmPos =
                    updateCircleDrag model.viewSize ( x, y ) model.alarmPos

                ( debounce, cmd ) =
                    Debounce.push debounceConfig alarmPos model.debounce
            in
            ( { model | alarmPos = alarmPos, debounce = debounce }, cmd )

        GotAlarm result ->
            case result of
                Ok ( alarmTime, alarmEnabled ) ->
                    ( { model | alarmPos = alarmToTracker alarmTime, alarmEnabled = alarmEnabled }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        DebounceMsg m ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast (saveAlarmTime model.alarmEnabled))
                        m
                        model.debounce
            in
            ( { model | debounce = debounce }, cmd )

        ViewChanged ( w, h ) ->
            ( { model | viewSize = ( w, h ) }, Cmd.none )

        Updated _ ->
            ( model, Cmd.none )

        ToggleChanged enabled ->
            ( { model
                | alarmEnabled = enabled
              }
            , saveAlarmTime enabled model.alarmPos
            )


trackerToAlarm : Float -> AlarmTime
trackerToAlarm tracker =
    let
        dur =
            Duration.seconds <| (tracker * (24 * 60 * 60) / 200)

        hours =
            toFloat <| floor <| Duration.inHours <| dur

        minutes =
            toFloat <| floor <| Duration.inMinutes <| Duration.hours (Duration.inHours dur - hours)
    in
    AlarmTime hours minutes


alarmToTracker : AlarmTime -> Float
alarmToTracker alarm =
    ((alarm.hour / 24) * 200) + ((1 / 24) * (alarm.minute / 60) * 200)


updateCircleDrag : ( Int, Int ) -> ( Float, Float ) -> Float -> Float
updateCircleDrag ( viewWidth, _ ) ( newX, newY ) existing =
    let
        centreInPage =
            ( ((toFloat viewWidth - 80) / 2) + 40, ((toFloat viewWidth - 80) / 2) + 50 )
    in
    let
        horizDiff =
            abs (Tuple.first centreInPage - newX)

        vertDiff =
            abs (Tuple.second centreInPage - newY)

        rightHalf =
            newX > Tuple.first centreInPage

        bottomHalf =
            newY > Tuple.second centreInPage
    in
    let
        offset =
            case ( rightHalf, bottomHalf ) of
                ( True, False ) ->
                    if existing >= 50 && existing <= 175 then
                        100

                    else
                        0

                ( True, True ) ->
                    if existing >= 75 then
                        100

                    else
                        0

                ( False, True ) ->
                    if existing >= 100 then
                        100

                    else
                        0

                ( False, False ) ->
                    if existing >= 150 || existing <= 25 then
                        100

                    else
                        0
    in
    case ( rightHalf, bottomHalf ) of
        ( True, False ) ->
            offset + ((abs <| atan (horizDiff / vertDiff)) / (pi / 2)) * 25

        ( True, True ) ->
            offset + 25 + (((pi / 2) - atan (horizDiff / vertDiff)) / (pi / 2)) * 25

        ( False, True ) ->
            offset + 50 + ((abs <| atan (horizDiff / vertDiff)) / (pi / 2)) * 25

        ( False, False ) ->
            offset + 75 + (((pi / 2) - atan (horizDiff / vertDiff)) / (pi / 2)) * 25



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Browser.Events.onResize onResize ]


onResize : Int -> Int -> Msg
onResize width height =
    ViewChanged ( width, height )



-- HTTP


saveAlarmTime : Bool -> Float -> Cmd Msg
saveAlarmTime isEnabled val =
    let
        alarm =
            trackerToAlarm val
    in
    Http.post
        { url = urlRoot ++ "/alarm"
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "hour", Json.Encode.float alarm.hour )
                    , ( "minute", Json.Encode.float alarm.minute )
                    , ( "enabled", Json.Encode.bool isEnabled )
                    ]
                )
        , expect = Http.expectWhatever Updated
        }


alarmDecoder : Decoder ( AlarmTime, Bool )
alarmDecoder =
    map3 (\a b c -> ( AlarmTime a b, c ))
        (field "hour" float)
        (field "minute" float)
        (field "enabled" bool)



-- View


view : Model -> List (Element Msg)
view model =
    [ html
        (svg
            [ viewBox "0 0 100 100"
            ]
            ([ circle
                [ cx "50%"
                , cy "50%"
                , r "46%"
                , fill "none"
                , strokeWidth "8%"
                , stroke <| colorToString <| colorC
                ]
                []
             ]
                ++ (if model.alarmEnabled then
                        (circleTracker >> singleton) model.alarmPos

                    else
                        []
                   )
            )
        )
    , Element.el
        [ Element.centerX
        , Font.size (scaled 12)
        , Element.paddingEach { top = scaled 8, right = 0, bottom = scaled 5, left = 0 }
        , Font.color colorA
        ]
        (Element.text <|
            if model.alarmEnabled then
                getAlarmTime model.alarmPos

            else
                "--:--"
        )
    , Element.row
        [ Element.width Element.fill
        , Element.paddingEach { top = scaled 8, right = 0, bottom = 0, left = 0 }
        , Border.widthEach { top = 1, right = 0, bottom = 0, left = 0 }
        , Border.color colorC
        ]
        [ Element.el
            [ Element.alignLeft
            , Font.light
            ]
            (Element.text "Enable alarm")
        , Element.el [ Element.alignRight ]
            (Input.checkbox [ Element.scale 2.3 ]
                { onChange = ToggleChanged
                , icon = Input.defaultCheckbox
                , checked = model.alarmEnabled
                , label = Input.labelHidden ""
                }
            )
        ]
    ]


circleTracker : Float -> Svg Msg
circleTracker loc =
    circle
        [ cx <| String.fromFloat <| 50 + 46 * (cos <| ((loc + 75) / 100) * 2 * pi)
        , cy <| String.fromFloat <| 50 + 46 * (sin <| ((loc + 75) / 100) * 2 * pi)
        , r "4%"
        , Touch.onMove (MoveAt << touchCoordinates)
        , fill <| colorToString <| colorD
        ]
        []


getAlarmTime : Float -> String
getAlarmTime progress =
    secondsToHoursMins <| round ((12 * 60 * 60) * (progress / 100))


secondsToHoursMins : Int -> String
secondsToHoursMins totalSeconds =
    let
        dur =
            Duration.seconds <| toFloat totalSeconds

        hours =
            floor <| Duration.inHours <| dur

        minutes =
            floor <| Duration.inMinutes <| Duration.hours (Duration.inHours dur - toFloat hours)
    in
    String.concat <|
        [ hours |> String.fromInt |> String.padLeft 2 '0'
        , ":"
        , minutes |> String.fromInt |> String.padLeft 2 '0'
        ]


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )
