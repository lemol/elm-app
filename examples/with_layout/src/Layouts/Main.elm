module Layouts.Main exposing (..)

import App.Routes as Routes exposing (toPath)
import App.Utils exposing (send, LayoutMsg(..))
import Browser exposing (Document)
import Global
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Random



-- DATA


type alias Page pageMsg =
    { title : String
    , headerTitle : Html pageMsg
    , main : Html pageMsg
    }



-- MODEL


type Color
    = Red
    | Green
    | Blue


type alias Model =
    { color : Color }


init : ( Model, Cmd Msg )
init =
    ( { color = Red
      }
    , Cmd.none
    )



-- MESSAGES


type Msg
    = Send Global.Msg
    | ShuffleColor
    | ColorChanged Color



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        Send globalMsg ->
            ( model, Cmd.none, send globalMsg )

        ShuffleColor ->
            ( model
            , Random.uniform Red [ Green, Blue ]
                |> Random.generate ColorChanged
            , Cmd.none
            )

        ColorChanged color ->
            ( { model | color = color }, Cmd.none, Cmd.none )



-- VIEW


view : Global.Model -> Model -> Page pageMsg -> Document (LayoutMsg Msg pageMsg)
view global model page =
    let
        title =
            page.title

        body =
            div
                []
                [ page.headerTitle
                    |> Html.map PageMsg
                , div
                    []
                    [ case global of
                        Nothing ->
                            button
                                [ onClick (BaseMsg <| Send <| Global.Login "Mercy Tchue") ]
                                [ text "Login" ]

                        Just _ ->
                            button
                                [ onClick (BaseMsg <| Send <| Global.Logout) ]
                                [ text "Logout for "
                                , text (Maybe.withDefault "" global)
                                ]
                    , button
                        [ onClick <| BaseMsg <| ShuffleColor ]
                        [ text "Shuffle Color" ]
                    ]
                , div
                    []
                    [ a
                        [ href (toPath Routes.Index) ]
                        [ text "Index" ]
                    , text " | "
                    , a
                        [ href (toPath Routes.Counter) ]
                        [ text "Counter" ]
                    , text " | "
                    , a
                        [ href (toPath Routes.CounterAsync) ]
                        [ text "Counter Async" ]
                    , text " | "
                    , a
                        [ href (toPath Routes.About) ]
                        [ text "About" ]
                    ]
                , div
                    [ style "background-color" (colorToHex model.color) ]
                    [ text "=========================="
                    , text "PAGE CONTENT STARTS HERE"
                    , text "=========================="
                    ]
                , page.main
                    |> Html.map PageMsg
                ]
    in
    { title = title
    , body = [ body ]
    }



-- UTILS


colorToHex : Color -> String
colorToHex color =
    case color of
        Red ->
            "#ff0000"

        Green ->
            "#00ff00"

        Blue ->
            "#0000ff"
