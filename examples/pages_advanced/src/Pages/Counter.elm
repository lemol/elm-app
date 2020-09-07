module Pages.Counter exposing (Model, Msg, init, update, view)

import App.Routes as Routes exposing (toPath)
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


view : Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "Counter Page" ]
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
