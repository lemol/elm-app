module SingleModule.JustViewTest exposing (..)

import Elm.Pretty
import ElmApp.Module as Module exposing (View(..))
import ElmApp.SingleModule.JustViewMainModule as JustViewMainModule
import ElmCodeGenUtils exposing (typedGeneric)
import Expect
import Test exposing (..)


withViewExposed : String
withViewExposed =
    """module App.Main exposing (main)

import Html exposing (Html)
import Main


main : Html msg
main =
    Main.view
"""


withMainExposed : String
withMainExposed =
    """module App.Main exposing (main)

import Html exposing (Html)
import Main


main : Html msg
main =
    Main.main
"""


suite : Test
suite =
    describe "SingleModule: Just View"
        [ test "withViewExposed" <|
            \_ ->
                let
                    module_ =
                        Module.build [ "Main" ]
                            |> Module.withView
                                (View_Document "view" (typedGeneric "Html" "msg"))

                    expected =
                        withViewExposed
                in
                JustViewMainModule.write "view" module_
                    |> Result.map (Elm.Pretty.pretty 120)
                    |> Expect.equal (Ok expected)
        , test "withMainExposed" <|
            \_ ->
                let
                    module_ =
                        Module.build [ "Main" ]
                            |> Module.withView
                                (View_Document "main" (typedGeneric "Html" "msg"))

                    expected =
                        withMainExposed
                in
                JustViewMainModule.write "main" module_
                    |> Result.map (Elm.Pretty.pretty 120)
                    |> Expect.equal (Ok expected)
        ]
