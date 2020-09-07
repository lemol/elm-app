module Pages.CounterAsync exposing (Model, Msg, init, subscriptions, update, view)

import App.Utils exposing (send)
import Global
import Html exposing (button, div, h1, text)
import Html.Events exposing (onClick)
import Layouts.Main as Layout
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
    | Send Global.Msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        Send globalMsg ->
            ( model, Cmd.none, send globalMsg )

        Increment ->
            ( model + 1
            , Cmd.none
            , Cmd.none
            )

        Reset ->
            ( 0
            , Cmd.none
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (always Increment)



-- VIEW


view : Global.Model -> Model -> Layout.Page Msg
view _ model =
    { title = "Counter Async"
    , headerTitle =
        h1
            []
            [ text "Counter Async Page" ]
    , main =
        div
            []
            [ text (String.fromInt model)
            , button
                [ onClick Reset
                ]
                [ text "reset"
                ]
            ]
    }
