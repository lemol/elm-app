module ParseCounter exposing (..)

import ElmApp.Module as Module exposing (Init(..), Model(..), Msg(..), Update(..), View(..))
import ElmApp.Parser
import ElmCodeGenUtils exposing (typeSimple, typedConcreteSimple)
import Expect
import Test exposing (..)
import Utils exposing (clearModuleNodeRange)


suite : Test
suite =
    describe "Counter! Single Page"
        [ test "with Msg -> Model -> Model" <|
            \_ ->
                let
                    result =
                        ElmApp.Parser.parseModule mainFile
                            |> Result.map clearModuleNodeRange

                    expected =
                        Module.build [ "Main" ]
                            |> Module.withModel
                                (Model1 "Model")
                            |> Module.withInit
                                (Init_Model "init" (typeSimple "Model"))
                            |> Module.withMsg
                                (Msg1 "Msg")
                            |> Module.withUpdate
                                (Update_Msg_Model_Model "update" (typeSimple "Msg") (typeSimple "Model") (typeSimple "Model"))
                            |> Module.withView
                                (View_Model_Document "view" (typeSimple "Model") (typedConcreteSimple "Html" "Msg"))
                in
                result |> Expect.equal (Ok expected)
        ]


mainFile : String
mainFile =
    """
module Main exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, text)
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
        [ button
            [ onClick Decrement ]
            [ text "-" ]
        , text (String.fromInt model)
        , button
            [ onClick Increment ]
            [ text "+" ]
        ]
"""
