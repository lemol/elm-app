module Spa.SpaBasic exposing (..)

import ElmApp.Error as Error exposing (Error)
import ElmApp.Module exposing (Init(..), Model(..), Msg(..), Subscriptions(..), Update(..), View(..))
import ElmApp.Spa as Spa exposing (Context)
import ElmApp.Spa.Config as Config exposing (DocumentInfo(..))
import Expect
import List.Extra
import Spa.Fixtures.BasicPages as BasicPages
import Spa.Fixtures.MainModule as MainModule
import Spa.Fixtures.PagesModule as PagesModule
import Test exposing (..)


context : Result Error Context
context =
    Spa.init
        { config = Config.default
        }
        |> Spa.addPage BasicPages.index
        |> Spa.addPage BasicPages.about
        |> Spa.addPage BasicPages.counter
        |> Spa.addPage BasicPages.counterAsync
        |> Spa.build


result : Result Error (List ( String, String ))
result =
    context |> Result.andThen Spa.write


getFile : String -> Result Error String
getFile name =
    result
        |> Result.andThen
            (\xs ->
                List.Extra.find (Tuple.first >> (==) name) xs
                    |> Maybe.map (Tuple.second >> Ok)
                    |> Maybe.withDefault (Err (Error.Error "file not found"))
            )


suite : Test
suite =
    describe "Spa"
        [ test "PagesModule" <|
            \_ ->
                getFile "App/Pages.elm"
                    |> Expect.equal (Ok PagesModule.content)
        , test "MainModule" <|
            \_ ->
                getFile "App/Main.elm"
                    |> Expect.equal (Ok MainModule.content)
        ]