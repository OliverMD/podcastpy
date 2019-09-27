module Main exposing (main)

import Browser
import Colours exposing (colorB, colorC, colorE)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Icon as Icon
import FontAwesome.Solid as SolidIcon
import Html exposing (Html)
import Page exposing (Page)
import Page.Alarm as Alarm
import Page.Player as Player
import Utils exposing (scaled)



-- Model


type Model
    = Player Player.Model
    | Alarm Alarm.Model


isPlayerPage : Model -> Bool
isPlayerPage model =
    case model of
        Player _ ->
            True

        _ ->
            False


isAlarmPage : Model -> Bool
isAlarmPage model =
    case model of
        Alarm _ ->
            True

        _ ->
            False


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( subModel, subCmd ) =
            Player.init
    in
    ( Player subModel, Cmd.map GotPlayerMsg subCmd )



-- Update


type Msg
    = GotPlayerMsg Player.Msg
    | GotAlarmMsg Alarm.Msg
    | ChangePage Page.Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangePage page, _ ) ->
            changePage page model

        ( GotAlarmMsg subMsg, Alarm alarmModel ) ->
            Alarm.update subMsg alarmModel
                |> updateWith Alarm GotAlarmMsg model

        ( GotPlayerMsg subMsg, Player playerModel ) ->
            Player.update subMsg playerModel
                |> updateWith Player GotPlayerMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


changePage : Page.Page -> Model -> ( Model, Cmd Msg )
changePage page model =
    case page of
        Page.Player ->
            Player.init
                |> updateWith Player GotPlayerMsg model

        Page.Alarm ->
            Alarm.init
                |> updateWith Alarm GotAlarmMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Player player ->
            Sub.map GotPlayerMsg (Player.subscriptions player)

        Alarm alarm ->
            Sub.map GotAlarmMsg (Alarm.subscriptions alarm)



-- View


view : Model -> Html Msg
view model =
    let
        viewPage a =
            Element.layoutWith { options = [ Element.focusStyle { borderColor = Nothing, backgroundColor = Nothing, shadow = Nothing } ] }
                []
                (Element.column [ Element.height Element.fill, Element.width Element.fill ]
                    [ Element.column
                        [ Element.width Element.fill, Element.height Element.fill, Element.paddingXY 40 50 ]
                        a
                    , navbarView model
                    ]
                )
    in
    case model of
        Player player ->
            viewPage <| List.map (Element.map GotPlayerMsg) (Player.view player)

        Alarm alarm ->
            viewPage <| List.map (Element.map GotAlarmMsg) (Alarm.view alarm)


navbarView : Model -> Element Msg
navbarView model =
    Element.el
        [ Element.centerX
        , Element.alignBottom
        , Element.width Element.fill
        , Background.color colorC
        , Element.paddingEach { top = scaled 2, right = 0, bottom = scaled 4, left = 0 }
        , Border.rounded 0
        ]
        (Element.row [ Element.centerX, Element.spacingXY 150 (scaled 1) ]
            [ Input.button
                [ Font.color
                    (if isPlayerPage model then
                        colorB

                     else
                        colorE
                    )
                ]
                { label =
                    Element.el [ Element.width <| Element.px <| scaled 7 ] <|
                        Element.html (Html.div [] [ Icon.view SolidIcon.play ])
                , onPress = Just (ChangePage Page.Player)
                }
            , Input.button
                [ Font.color
                    (if isAlarmPage model then
                        colorB

                     else
                        colorE
                    )
                ]
                { label =
                    Element.el [ Element.width <| Element.px <| scaled 7 ] <|
                        Element.html (Html.div [] [ Icon.view SolidIcon.bell ])
                , onPress = Just (ChangePage Page.Alarm)
                }
            ]
        )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
