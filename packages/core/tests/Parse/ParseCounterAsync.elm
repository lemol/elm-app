module Parse.ParseCounterAsync exposing (..)

import Elm.CodeGen exposing (tupleAnn)
import ElmApp.Module as Module exposing (Init(..), Model(..), Msg(..), Subscriptions(..), Update(..), View(..))
import ElmApp.Parser
import ElmCodeGenUtils exposing (typeSimple, typedConcreteSimple)
import Expect
import Test exposing (..)
import Utils exposing (clearModuleNodeRange)


suite : Test
suite =
    describe "Single Page: CounterAsync"
        [ test "with Msg -> Model -> (Model, Cmd Msg)" <|
            \_ ->
                let
                    result =
                        ElmApp.Parser.parseModule mainFile
                            |> Result.map clearModuleNodeRange

                    modelCmd =
                        tupleAnn [ typeSimple "Model", typedConcreteSimple "Cmd" "Msg" ]

                    expected =
                        Module.build [ "Main" ]
                            |> Module.withModel
                                (Model1 "Model")
                            |> Module.withInit
                                (Init_ModelCmd "init" modelCmd)
                            |> Module.withMsg
                                (Msg1 "Msg")
                            |> Module.withUpdate
                                (Update_Msg_Model_ModelCmd "update" (typeSimple "Msg") (typeSimple "Model") modelCmd)
                            |> Module.withSubscriptions
                                (Subscriptions_Model_Sub "subscriptions" (typeSimple "Model") (typedConcreteSimple "Sub" "Msg"))
                            |> Module.withView
                                (View_Model_Document "view" (typeSimple "Model") (typedConcreteSimple "Html" "Msg"))
                in
                result
                    |> Expect.equal (Ok expected)
        ]


mainFile : String
mainFile =
    """
module Main exposing (Model, Msg, subscriptions, update, view, init)

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
            [ text "reset"
            ]
        ]
"""
