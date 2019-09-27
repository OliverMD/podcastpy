module Page.Player exposing (Model, Msg, init, subscriptions, update, view)

import Colours exposing (colorA, colorC, colorD)
import Duration
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (thumb)
import FontAwesome.Icon as Icon
import FontAwesome.Regular as Regular
import Html
import Http
import Json.Decode exposing (Decoder, bool, field, float, int, map4)
import Time exposing (Posix)
import Utils exposing (floatFlip, microTickLenMs, scaled, urlRoot)



-- Model


type PodcastState
    = Ready
    | Paused
    | Playing


type alias Model =
    { volume : Int
    , elapsedTimeMs : Int
    , progress : Float
    , playerState : PodcastState
    , length : Int
    , imageUrl : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { volume = 0, elapsedTimeMs = 0, progress = 0, playerState = Ready, length = 0, imageUrl = Nothing }
    , Cmd.batch
        [ Http.get
            { url = urlRoot ++ "/volume"
            , expect = Http.expectString VolumeIs
            }
        , Http.get { url = urlRoot ++ "/image", expect = Http.expectString GotImageUrl }
        ]
    )



-- Update


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VolumeIs result ->
            case result of
                Ok val ->
                    ( { model | volume = Maybe.withDefault 0 (String.toInt val) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        Updated _ ->
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

                Err _ ->
                    ( model, Cmd.none )

        GotImageUrl result ->
            case result of
                Ok url ->
                    ( { model | imageUrl = Just url }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )


nextPlayerState : PodcastState -> PodcastState
nextPlayerState playerState =
    case playerState of
        Playing ->
            Paused

        Paused ->
            Playing

        Ready ->
            Playing



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every microTickLenMs MicroTick -- No HTTP calls --
        , Time.every 2000 Tick
        ]



-- HTTP


type alias PlayerState =
    { progress : Float
    , time : Int
    , length : Int
    , paused : Bool
    }


stateDecoder : Decoder PlayerState
stateDecoder =
    map4 PlayerState
        (field "progress" float)
        (field "time" int)
        (field "length" int)
        (field "paused" bool)



-- View


view : Model -> List (Element Msg)
view model =
    [ podcastImage model
    , Element.column [ Element.alignBottom, Element.width Element.fill, Element.height Element.fill ]
        [ progressBar model
        , playButton model
        , volumeBar model
        ]
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
                , Font.color colorA
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
                , Background.color colorC
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
                , Element.alpha 1
                , Font.color <| colorA
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
                , Border.width 0
                , Border.color (Element.rgb 0.5 0.5 0.5)
                , Background.color colorD
                ]
        }


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
                , Background.color colorC
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
                , Element.alpha 1
                , Font.color colorA
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
                , Border.width 0
                , Border.color (Element.rgb 0.5 0.5 0.5)
                , Background.color colorD
                ]
        }


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
