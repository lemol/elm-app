module Spa.Fixtures.BasicPages exposing (..)


index : String
index =
    """module Pages.Index exposing (view)

import App.Pages exposing (AppUrls, urlString)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href)


view : { urls : AppUrls } -> Html msg
view =
    div
        []
        [ h1
            []
            [ text "Index Page" ]
        , div
            []
            [ a
                [ href (urlString urls.index) ]
                [ text "Index" ]
            , text " | "
            , a
                [ href (urlString urls.counter) ]
                [ text "Counter" ]
            , text " | "
            , a
                [ href (urlString urls.counterAsync) ]
                [ text "Counter Async" ]
            , text " | "
            , a
                [ href (urlString urls.about) ]
                [ text "About" ]
            ]
        ]

    """

about : String
about =
    """module Pages.About exposing (view)

import App.Pages exposing (AppUrls, urlString)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href)


view : { urls : AppUrls } -> Html msg
view =
    div
        []
        [ h1
            []
            [ text "About Page" ]
        , div
            []
            [ a
                [ href (urlString urls.index) ]
                [ text "Index" ]
            , text " | "
            , a
                [ href (urlString urls.counter) ]
                [ text "Counter" ]
            , text " | "
            , a
                [ href (urlString urls.counterAsync) ]
                [ text "Counter Async" ]
            , text " | "
            , a
                [ href (urlString urls.about) ]
                [ text "About" ]
            ]
        ]

    """


counter : String
counter =
    """module Pages.Counter exposing (Model, Msg, init, update, view)

import App.Pages exposing (AppUrls, urlString)
import Html exposing (Html, a, button, div, h1, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- MESSAGES


type Msg
    = Increment
    | Decrement



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1



-- VIEW


view : { urls : AppUrls } -> Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "Index Page" ]
        , div
            []
            [ a
                [ href (urlString urls.index) ]
                [ text "Index" ]
            , text " | "
            , a
                [ href (urlString urls.counter) ]
                [ text "Counter" ]
            , text " | "
            , a
                [ href (urlString urls.counterAsync) ]
                [ text "Counter Async" ]
            , text " | "
            , a
                [ href (urlString urls.about) ]
                [ text "About" ]
            ]
        , div
            []
            [ button
                [ onClick Decrement ]
                [ text "-" ]
            , text (String.fromInt model)
            , button
                [ onClick Increment ]
                [ text "+" ]
            ]
        ]

    """


counterAsync : String
counterAsync =
    """module Pages.CounterAsync exposing (Model, Msg, init, subscriptions, update, view)

import App.Pages exposing (AppUrls, urlString)
import Html exposing (Html, a, button, div, h1, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Time



-- MODEL


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0
    , Cmd.none
    )



-- MESSAGES


type Msg
    = Increment
    | Reset



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1
            , Cmd.none
            )

        Reset ->
            ( 0
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (always Increment)



-- VIEW


view : { urls : AppUrls } -> Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "Index Page" ]
        , div
            []
            [ a
                [ href (urlString urls.index) ]
                [ text "Index" ]
            , text " | "
            , a
                [ href (urlString urls.counter) ]
                [ text "Counter" ]
            , text " | "
            , a
                [ href (urlString urls.counterAsync) ]
                [ text "Counter Async" ]
            , text " | "
            , a
                [ href (urlString urls.about) ]
                [ text "About" ]
            ]
        , div
            []
            [ text (String.fromInt model)
            , button
                [ onClick Reset
                ]
                [ text "reset"
                ]
            ]
        ]

    """