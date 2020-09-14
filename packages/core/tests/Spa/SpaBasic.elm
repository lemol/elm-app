module Spa.SpaBasic exposing (..)

import ElmApp.Error as Error exposing (Error)
import ElmApp.Module exposing (Init(..), Model(..), Msg(..), Subscriptions(..), Update(..), View(..))
import ElmApp.Spa as Spa exposing (Context)
import ElmApp.Spa.Config exposing (DocumentInfo(..))
import Expect
import Json.Encode as Json
import List.Extra
import Spa.Fixtures.BasicPages as BasicPages
import Spa.Fixtures.BasicPages.MainModule as MainModule
import Spa.Fixtures.BasicPages.PagesModule as PagesModule
import Spa.Fixtures.BasicPages.RouterModule as RouterModule
import Test exposing (..)


configSimple : Json.Value
configSimple =
    Json.object
        [ ( "routes"
          , Json.list Json.object
                [ [ ( "path", Json.string "/" )
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


configWithRouteParams : Json.Value
configWithRouteParams =
    Json.object
        [ ( "routes"
          , Json.list Json.object
                [ [ ( "path", Json.string "/" )
                  , ( "module", Json.string "Pages.Index" )
                  ]
                , [ ( "path", Json.string "/about/:countryId/users/:userId:String" )
                  , ( "module", Json.string "Pages.About" )
                  ]
                , [ ( "path", Json.string "/counter" )
                  , ( "module", Json.string "Pages.Counter" )
                  ]
                , [ ( "path", Json.string "/counter/async/:name:String/:initial:Int/:step:Int/go" )
                  , ( "module", Json.string "Pages.CounterAsync" )
                  ]
                ]
          )
        ]


suite : Test
suite =
    describe "basic spa"
        [ basicSuite "simple pages"
            configSimple
            { mainModule = MainModule.content
            , pagesModule = PagesModule.contentSimple
            , routerModule = RouterModule.contentSimple
            }
        , basicSuite "with route params"
            configWithRouteParams
            { mainModule = MainModule.content
            , pagesModule = PagesModule.contentWithRouteParams
            , routerModule = RouterModule.contentWithRouteParams
            }
        ]


basicSuite :
    String
    -> Json.Value
    ->
        { pagesModule : String
        , mainModule : String
        , routerModule : String
        }
    -> Test
basicSuite name config content =
    describe name
        [ test "fail if routes are not satisfied" <|
            \_ ->
                Spa.initDecodingConfig config
                    |> Spa.addPage BasicPages.about
                    |> Spa.addPage BasicPages.counter
                    |> Spa.build
                    |> Expect.err
        , test "should generate the App/Pages.elm file" <|
            \_ ->
                justGetFile config "App/Pages.elm"
                    |> Expect.equal (Ok content.pagesModule)
        , test "should generate the App/Main.elm file" <|
            \_ ->
                justGetFile config "App/Main.elm"
                    |> Expect.equal (Ok content.mainModule)
        , test "should generate the App/Pages/Internal/Router.elm file" <|
            \_ ->
                justGetFile config "App/Pages/Internal/Router.elm"
                    |> Expect.equal (Ok content.routerModule)
        ]


context : Json.Value -> Result Error Context
context config =
    Spa.initDecodingConfig config
        |> Spa.addPage BasicPages.about
        |> Spa.addPage BasicPages.counter
        |> Spa.addPage BasicPages.counterAsync
        |> Spa.addPage BasicPages.index
        |> Spa.build


result : Result Error Context -> Result Error (List ( String, String ))
result =
    Result.andThen Spa.write


getFile : String -> Result Error (List ( String, String )) -> Result Error String
getFile name =
    Result.andThen
        (\xs ->
            List.Extra.find (Tuple.first >> (==) name) xs
                |> Maybe.map (Tuple.second >> Ok)
                |> Maybe.withDefault (Err (Error.Error "file not found"))
        )


justGetFile : Json.Value -> String -> Result Error String
justGetFile config name =
    context config
        |> result
        |> getFile name
