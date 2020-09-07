module Pages.Counter exposing (Model, Msg, init, update, view)

import App.Routes as Routes exposing (toPath)
import App.Utils exposing (send)
import Global
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


view : Global.Model -> Model -> Html Msg
view global model =
    div
        []
        [ h1
            []
            [ text "Counter Page" ]
        , div
            []
            [ case global of
                Nothing ->
                    button
                        [ onClick (Send <| Global.Login "Mercy Tchue") ]
                        [ text "Login" ]

                Just _ ->
                    button
                        [ onClick (Send <| Global.Logout) ]
                        [ text "Logout for "
                        , text (Maybe.withDefault "" global)
                        ]
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
