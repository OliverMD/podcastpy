module Main exposing (Model)

import Browser
import Browser.Dom
import Browser.Events
import Debounce
import Duration
import Element exposing (Color, Element, html, modular, rgb255, rgba255)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (Thumb, thumb)
import FontAwesome.Icon as Icon
import FontAwesome.Regular as Regular
import FontAwesome.Solid as SolidIcon
import Html exposing (Html)
import Html.Events.Extra.Touch as Touch
import Http
import Json.Decode exposing (Decoder, bool, field, float, int, map2, map4)
import Json.Encode
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task
import Time exposing (Posix)


urlRoot =
    ""


microTickLenMs =
    500


colorA =
    rgba255 217 149 15 1


colorB =
    rgba255 255 239 207 1


colorC =
    rgba255 255 204 102 1


colorD =
    rgba255 217 232 255 1


colorE =
    rgba255 12 43 89 1


type PodcastState
    = Ready
    | Paused
    | Playing


type Page
    = Player
    | Alarms


type alias Model =
    { volume : Int
    , elapsedTimeMs : Int
    , progress : Float
    , playerState : PodcastState
    , length : Int
    , imageUrl : Maybe String
    , page : Page
    , alarmPos : Float
    , debounce : Debounce.Debounce Float
    , viewSize : ( Int, Int )
    }


type alias PlayerState =
    { progress : Float
    , time : Int
    , length : Int
    , paused : Bool
    }


type alias Alarm =
    { hour : Float
    , minute : Float
    }


type Msg
    = VolumeIs (Result Http.Error String)
    | Updated (Result Http.Error ())
    | ChangeVolume Int
    | PlayPodcast
    | Ignore
    | Tick Posix
    | MicroTick Posix
    | GotState (Result Http.Error PlayerState)
    | GotImageUrl (Result Http.Error String)
    | PlayPage
    | AlarmPage
    | MoveAt ( Float, Float )
    | GotAlarm (Result Http.Error Alarm)
    | DebounceMsg Debounce.Msg
    | ViewChanged ( Int, Int )


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = DebounceMsg
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { volume = 0
      , elapsedTimeMs = 0
      , progress = 0.0
      , playerState = Ready
      , length = 0
      , imageUrl = Nothing
      , page = Alarms
      , alarmPos = 0
      , debounce = Debounce.init
      , viewSize = ( 0, 0 )
      }
    , Cmd.batch
        [ Http.get
            { url = urlRoot ++ "/volume"
            , expect = Http.expectString VolumeIs
            }
        , Http.get { url = urlRoot ++ "/image", expect = Http.expectString GotImageUrl }
        , Http.get { url = urlRoot ++ "/alarm", expect = Http.expectJson GotAlarm alarmDecoder }
        , Task.perform (\v -> ViewChanged ( round v.viewport.width, round v.viewport.height )) Browser.Dom.getViewport
        ]
    )


alarmDecoder : Decoder Alarm
alarmDecoder =
    map2 Alarm
        (field "hour" float)
        (field "minute" float)


stateDecoder : Decoder PlayerState
stateDecoder =
    map4 PlayerState
        (field "progress" float)
        (field "time" int)
        (field "length" int)
        (field "paused" bool)


nextPlayerState : PodcastState -> PodcastState
nextPlayerState playerState =
    case playerState of
        Playing ->
            Paused

        Paused ->
            Playing

        Ready ->
            Playing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VolumeIs result ->
            case result of
                Ok val ->
                    ( { model | volume = Maybe.withDefault 0 (String.toInt val) }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        Updated result ->
            ( model, Http.get { url = urlRoot ++ "/image", expect = Http.expectString GotImageUrl } )

        ChangeVolume val ->
            ( { model | volume = val }
            , Http.post
                { url = urlRoot ++ "/volume?vol=" ++ String.fromInt val
                , body = Http.emptyBody
                , expect = Http.expectWhatever Updated
                }
            )

        Ignore ->
            ( model, Cmd.none )

        Tick _ ->
            ( model
            , Http.get
                { url = urlRoot ++ "/state"
                , expect = Http.expectJson GotState stateDecoder
                }
            )

        MicroTick _ ->
            ( { model
                | elapsedTimeMs =
                    model.elapsedTimeMs
                        + (if model.playerState == Playing then
                            microTickLenMs

                           else
                            0
                          )
              }
            , Cmd.none
            )

        PlayPodcast ->
            ( { model | playerState = nextPlayerState model.playerState }
            , Http.get
                { url =
                    urlRoot
                        ++ (if model.playerState == Ready then
                                "/play"

                            else
                                "/pause"
                           )
                , expect = Http.expectWhatever Updated
                }
            )

        GotState result ->
            case result of
                Ok state ->
                    ( { model
                        | progress = state.progress
                        , elapsedTimeMs = state.time * 1000
                        , length = state.length
                        , playerState =
                            if state.length == 0 then
                                Ready

                            else if state.paused == True then
                                Paused

                            else
                                Playing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )

        GotImageUrl result ->
            case result of
                Ok url ->
                    ( { model | imageUrl = Just url }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        PlayPage ->
            ( { model | page = Player }, Cmd.none )

        AlarmPage ->
            ( { model | page = Alarms }, Cmd.none )

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
                Ok value ->
                    ( { model | alarmPos = alarmToTracker value }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )

        DebounceMsg m ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast save)
                        m
                        model.debounce
            in
            ( { model | debounce = debounce }, cmd )

        ViewChanged ( w, h ) ->
            ( { model | viewSize = ( w, h ) }, Cmd.none )


save : Float -> Cmd Msg
save val =
    let
        alarm =
            trackerToAlarm val
    in
    Http.post
        { url = urlRoot ++ "/alarm"
        , body = Http.jsonBody (Json.Encode.object [ ( "hour", Json.Encode.float alarm.hour ), ( "minute", Json.Encode.float alarm.minute ) ])
        , expect = Http.expectWhatever Updated
        }


trackerToAlarm : Float -> Alarm
trackerToAlarm tracker =
    let
        dur =
            Duration.seconds <| (tracker * (24 * 60 * 60) / 200)

        hours =
            toFloat <| floor <| Duration.inHours <| dur

        minutes =
            toFloat <| floor <| Duration.inMinutes <| Duration.hours (Duration.inHours dur - hours)
    in
    Alarm hours minutes


alarmToTracker : Alarm -> Float
alarmToTracker alarm =
    ((alarm.hour / 24) * 200) + ((1 / 24) * (alarm.minute / 60) * 200)



-- This assumes that the svg element is at the top of the page to get the location of the centre with respect to the
-- page coordinates.


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



-- VIEW --


scaled : Int -> Int
scaled =
    round << modular 8 1.25


view : Model -> Html Msg
view model =
    Element.layoutWith { options = [ Element.focusStyle { borderColor = Nothing, backgroundColor = Nothing, shadow = Nothing } ] }
        []
        (Element.column [ Element.height Element.fill, Element.width Element.fill ]
            [ Element.column
                [ Element.width Element.fill, Element.height Element.fill, Element.paddingXY 40 50 ]
                (getPage model)
            , getNavBar model
            ]
        )


getNavBar : Model -> Element Msg
getNavBar model =
    Element.el
        [ Element.centerX
        , Element.alignBottom
        , Element.width Element.fill
        , Background.color colorD
        , Element.paddingEach { top = scaled 2, right = 0, bottom = scaled 4, left = 0 }
        , Border.rounded 0
        ]
        (Element.row [ Element.centerX, Element.spacingXY 150 (scaled 1) ]
            [ Input.button [ Font.color colorE ]
                { label =
                    Element.el [ Element.width <| Element.px <| scaled 7 ] <|
                        Element.html (Html.div [] [ Icon.view SolidIcon.play ])
                , onPress = Just PlayPage
                }
            , Input.button [ Font.color colorE ]
                { label =
                    Element.el [ Element.width <| Element.px <| scaled 7 ] <|
                        Element.html (Html.div [] [ Icon.view SolidIcon.bell ])
                , onPress = Just AlarmPage
                }
            ]
        )


getPage : Model -> List (Element Msg)
getPage model =
    case model.page of
        Player ->
            podcastPageView model

        Alarms ->
            alarmsPageView model


podcastPageView : Model -> List (Element Msg)
podcastPageView model =
    [ podcastImage model
    , Element.column [ Element.alignBottom, Element.width Element.fill, Element.height Element.fill ]
        [ progressBar model
        , playButton model
        , volumeBar model
        ]
    ]


circleTracker : Float -> Svg Msg
circleTracker loc =
    circle
        [ cx <| String.fromFloat <| 50 + 46 * (cos <| ((loc + 75) / 100) * 2 * pi)
        , cy <| String.fromFloat <| 50 + 46 * (sin <| ((loc + 75) / 100) * 2 * pi)
        , r "4%"
        , Touch.onMove (MoveAt << touchCoordinates)
        , fill "#FFCC66"
        ]
        []


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


getAlarmTime : Float -> String
getAlarmTime progress =
    secondsToHoursMins <| round ((12 * 60 * 60) * (progress / 100))


alarmsPageView : Model -> List (Element Msg)
alarmsPageView model =
    [ html
        (svg
            [ viewBox "0 0 100 100"
            ]
            [ circle
                [ cx "50%"
                , cy "50%"
                , r "46%"
                , fill "none"
                , strokeWidth "8%"
                , stroke "#D9E8FF"
                ]
                []
            , circleTracker model.alarmPos
            ]
        )
    , Element.el
        [ Element.centerX
        , Font.size (scaled 12)
        , Element.paddingEach { top = scaled 10, right = 0, bottom = 0, left = 0 }
        , Font.color colorD
        ]
        (Element.text (getAlarmTime model.alarmPos))
    ]


podcastImage : Model -> Element Msg
podcastImage model =
    Element.image
        [ Element.height Element.fill
        , Element.width Element.fill
        , Element.paddingEach { top = 0, right = 0, bottom = scaled 2, left = 0 }
        , Element.centerY
        ]
        { src = Maybe.withDefault "" model.imageUrl, description = "Hi" }


playButton : Model -> Element Msg
playButton model =
    Input.button
        [ Element.centerX
        , Element.paddingXY 0 (scaled 3)
        ]
        { label =
            Element.el
                [ Element.centerX
                , Element.width <| Element.px <| scaled 10
                , Font.color colorC
                ]
            <|
                Element.html
                    (Html.div []
                        [ if model.playerState == Playing then
                            Icon.view Regular.pauseCircle

                          else
                            Icon.view Regular.playCircle
                        ]
                    )
        , onPress = Just PlayPodcast
        }


volumeBar : Model -> Element Msg
volumeBar model =
    Input.slider
        [ Element.height (scaled 1 |> Element.px)
        , Element.width Element.fill
        , Element.alignBottom

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (scaled -1 |> Element.px)
                , Element.centerY
                , Background.color colorB
                , Border.rounded 10
                ]
                Element.none
            )
        ]
        { onChange = round >> ChangeVolume
        , label =
            Input.labelAbove
                [ Element.centerX
                , Font.size (scaled 5)
                , Element.alpha 0.5
                , Font.color <| colorD
                ]
                (model.volume |> String.fromInt |> Element.text)
        , min = 0
        , max = 100
        , step = Just 1
        , value = toFloat model.volume
        , thumb =
            thumb
                [ Element.width (Element.px (scaled 5))
                , Element.height (Element.px (scaled 5))
                , Border.rounded 40
                , Border.width 1
                , Border.color (Element.rgb 0.5 0.5 0.5)
                , Background.color colorC
                ]
        }


secondsToText : Int -> String
secondsToText totalSeconds =
    let
        dur =
            Duration.seconds <| toFloat totalSeconds

        hours =
            floor <| Duration.inHours <| dur

        minutes =
            floor <| Duration.inMinutes <| Duration.hours (Duration.inHours dur - toFloat hours)

        seconds =
            floor <| Duration.inSeconds <| Duration.minutes (Duration.inMinutes dur - toFloat minutes)
    in
    String.concat <|
        [ hours |> String.fromInt |> String.padLeft 2 '0'
        , ":"
        , minutes |> String.fromInt |> String.padLeft 2 '0'
        , ":"
        , seconds |> String.fromInt |> String.padLeft 2 '0'
        ]


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



-- Reverses the argument order for 2-arity function --


floatFlip : (Float -> Float -> Float) -> Float -> (Float -> Float)
floatFlip function den =
    \x -> function x den


getElapsedTime : Model -> String
getElapsedTime model =
    let
        actualElapsedSeconds =
            if model.playerState == Playing || model.playerState == Paused then
                model.elapsedTimeMs |> toFloat |> floatFlip (/) 1000 |> round

            else
                0
    in
    secondsToText actualElapsedSeconds ++ " / " ++ secondsToText model.length


progressBar : Model -> Element Msg
progressBar model =
    Input.slider
        [ Element.height (scaled 1 |> Element.px)
        , Element.width Element.fill
        , Element.alignBottom

        -- Here is where we're creating/styling the "track"
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (scaled -2 |> Element.px)
                , Element.centerY
                , Background.color colorB
                , Border.rounded 10
                ]
                Element.none
            )
        ]
        { onChange = \_ -> Ignore
        , label =
            Input.labelAbove
                [ Element.centerX
                , Font.size (scaled 5)
                , Element.alpha 0.5
                , Font.color colorD
                ]
                (model |> getElapsedTime |> Element.text)
        , min = 0
        , max = 1
        , step = Just 0.0001
        , value = model.progress
        , thumb =
            thumb
                [ Element.width (Element.px (scaled 4))
                , Element.height (Element.px (scaled 4))
                , Border.rounded 40
                , Border.width 1
                , Border.color (Element.rgb 0.5 0.5 0.5)
                , Background.color colorA
                ]
        }


onResize : Int -> Int -> Msg
onResize width height =
    ViewChanged ( width, height )



-- PROGRAM --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every microTickLenMs MicroTick -- No HTTP calls --
        , Time.every 2000 Tick
        , Browser.Events.onResize onResize
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
