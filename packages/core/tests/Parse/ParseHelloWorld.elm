module Parse.ParseHelloWorld exposing (..)

import ElmApp.Module as Module exposing (View(..))
import ElmApp.Parser
import ElmCodeGenUtils exposing (typedGeneric)
import Expect
import Test exposing (..)
import Utils exposing (clearModuleNodeRange)


mainFileExposeView : String
mainFileExposeView =
    """module Main exposing (view)

import Html exposing (Html, text)

view : Html msg
view =
    Html.text "Hello world!"
"""


mainFileExposeMain : String
mainFileExposeMain =
    """module Main exposing (main)

import Html exposing (Html, text)

main : Html msg
main =
    Html.text "Hello world!"
"""


suite : Test
suite =
    describe "Hello world! Simple case"
        [ test "hello world exposing view : Html msg" <|
            \_ ->
                let
                    result =
                        ElmApp.Parser.parseModule mainFileExposeView
                            |> Result.map clearModuleNodeRange

                    expected =
                        Module.build [ "Main" ]
                            |> Module.withView
                                (View_Document "view" (typedGeneric "Html" "msg"))
                in
                result
                    |> Expect.equal
                        (Ok expected)
        , test "hello world exposing main : Html msg" <|
            \_ ->
                let
                    result =
                        ElmApp.Parser.parseModule mainFileExposeMain
                            |> Result.map clearModuleNodeRange

                    expected =
                        Module.build [ "Main" ]
                            |> Module.withView
                                (View_Document "main" (typedGeneric "Html" "msg"))
                in
                result
                    |> Expect.equal (Ok expected)
        ]
