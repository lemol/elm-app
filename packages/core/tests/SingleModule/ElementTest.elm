module SingleModule.ElementTest exposing (..)

import Elm.CodeGen exposing (tupleAnn)
import ElmApp.Module as Module exposing (Init(..), Model(..), Msg(..), Subscriptions(..), Update(..), View(..))
import ElmApp.SingleModule as SingleModule
import ElmCodeGenUtils exposing (typeSimple, typedConcreteSimple)
import Expect
import Test exposing (..)


elementMain : String
elementMain =
    """module App.Main exposing (main)

import Browser
import Main


main : Program () Main.Model Main.Msg
main =
    Browser.element
        { init = always Main.init, update = Main.update, view = Main.view, subscriptions = Main.subscriptions }
"""


suite : Test
suite =
    describe "SingleModule: Element"
        [ test "elementMain" <|
            \_ ->
                let
                    modelCmd =
                        tupleAnn [ typeSimple "Model", typedConcreteSimple "Cmd" "Msg" ]

                    module_ =
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

                    expected =
                        ( "Main.elm", elementMain )
                in
                SingleModule.write module_
                    |> Expect.equal (Ok expected)
        ]
