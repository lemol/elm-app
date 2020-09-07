module Pages.CounterAsync exposing (Model, Msg, init, subscriptions, update, view)

import App.Routes as Routes exposing (toPath)
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


view : Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "Counter Async Page" ]
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
            [ text (String.fromInt model)
            , button
                [ onClick Reset
                ]
                [ text "reset"
                ]
            ]
        ]
