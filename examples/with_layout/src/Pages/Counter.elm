module Pages.Counter exposing (Model, Msg, init, update, view)

import App.Routes as Routes exposing (toPath)
import App.Utils exposing (send)
import Global
import Html exposing (Html, a, button, div, h1, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Layouts.Main as Layout



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
    | Send Global.Msg



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        Send globalMsg ->
            ( model, Cmd.none, send globalMsg )

        Increment ->
            ( model + 1, Cmd.none, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none, Cmd.none )



-- VIEW


view : Global.Model -> Model -> Layout.Page Msg
view _ model =
    { title = "Counter"
    , headerTitle =
        h1
            []
            [ text "Counter Page" ]
    , main =
        div
            []
            [ button
                [ onClick Decrement ]
                [ text "-" ]
            , text (String.fromInt model)
            , button
                [ onClick Increment ]
                [ text "+" ]
            ]
    }

