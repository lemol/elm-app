module Index exposing (Model, Msg, subscriptions, update, view, init)

import Html exposing (Html, button, div, text)
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
subscriptions model =
    Time.every 1000 (always Increment)



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ text (String.fromInt model)
        , button
            [ onClick Reset
            ]
            [ text "reset2 again outer"
            ]
        ]
