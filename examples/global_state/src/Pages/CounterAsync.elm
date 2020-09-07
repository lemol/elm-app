module Pages.CounterAsync exposing (Model, Msg, init, subscriptions, update, view)

import App.Routes as Routes exposing (toPath)
import App.Utils exposing (send)
import Global
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


view : Global.Model -> Model -> Html Msg
view global model =
    div
        []
        [ h1
            []
            [ text "Counter Async Page" ]
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
            [ text (String.fromInt model)
            , button
                [ onClick Reset
                ]
                [ text "reset"
                ]
            ]
        ]
