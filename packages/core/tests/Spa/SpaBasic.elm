module Spa.SpaBasic exposing (..)

import ElmApp.Error as Error exposing (Error)
import ElmApp.Module exposing (Init(..), Model(..), Msg(..), Subscriptions(..), Update(..), View(..))
import ElmApp.Spa as Spa exposing (Context)
import ElmApp.Spa.Config exposing (DocumentInfo(..))
import Expect
import Json.Encode as Json
import List.Extra
import Spa.Fixtures.BasicPages as BasicPages
import Spa.Fixtures.MainModule as MainModule
import Spa.Fixtures.PagesModule as PagesModule
import Test exposing (..)


config : Json.Value
config =
    Json.object
        [ ( "routes"
          , Json.list Json.object
                [ [ ( "path", Json.string "/index" )
                  , ( "module", Json.string "Pages.Index" )
                  ]
                , [ ( "path", Json.string "/about" )
                  , ( "module", Json.string "Pages.About" )
                  ]
                , [ ( "path", Json.string "/counter" )
                  , ( "module", Json.string "Pages.Counter" )
                  ]
                , [ ( "path", Json.string "/counter/async" )
                  , ( "module", Json.string "Pages.CounterAsync" )
                  ]
                ]
          )
        ]


context : Result Error Context
context =
    Spa.initDecodingConfig config
        |> Spa.addPage BasicPages.about
        |> Spa.addPage BasicPages.counter
        |> Spa.addPage BasicPages.counterAsync
        |> Spa.addPage BasicPages.index
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
        [ test "fail if routes are not satisfied" <|
            \_ ->
                Spa.initDecodingConfig config
                    |> Spa.addPage BasicPages.about
                    |> Spa.addPage BasicPages.counter
                    |> Spa.build
                    |> Expect.err
        , test "PagesModule" <|
            \_ ->
                getFile "App/Pages.elm"
                    |> Expect.equal (Ok PagesModule.content)
        , test "MainModule" <|
            \_ ->
                getFile "App/Main.elm"
                    |> Expect.equal (Ok MainModule.content)
        ]
