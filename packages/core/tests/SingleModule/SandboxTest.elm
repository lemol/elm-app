module SingleModule.SandboxTest exposing (..)

import Elm.Pretty
import ElmApp.Module as Module exposing (Init(..), Model(..), Msg(..), Update(..), View(..))
import ElmApp.SingleModule.SandboxMainModule as SandboxMainModule
import ElmCodeGenUtils exposing (typeSimple, typedConcreteSimple, typedGeneric)
import Expect
import Test exposing (..)


sandboxMain : String
sandboxMain =
    """module App.Main exposing (main)

import Browser
import Main


main : Program () Main.Model Main.Msg
main =
    Browser.sandbox { init = Main.init, update = Main.update, view = Main.view }
"""


suite : Test
suite =
    describe "SingleModule: Sandbox"
        [ test "sandboxMain" <|
            \_ ->
                let
                    module_ =
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

                    expected =
                        sandboxMain
                in
                SandboxMainModule.write module_
                    |> Result.map (Elm.Pretty.pretty 120)
                    |> Expect.equal (Ok expected)
        ]
