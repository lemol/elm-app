module Spa.SpaBasic exposing (..)

import ElmApp.Error as Error exposing (Error)
import ElmApp.Module exposing (Init(..), Model(..), Msg(..), Subscriptions(..), Update(..), View(..))
import ElmApp.Spa as Spa exposing (Context)
import ElmApp.Spa.Config exposing (DocumentInfo(..))
import Expect
import Json.Encode as Json
import List.Extra
import Spa.Fixtures.BasicPages as BasicPages
import Spa.Fixtures.BasicPages.Main_Elm as Main_Elm
import Spa.Fixtures.BasicPages.Pages_Elm as Pages_Elm
import Spa.Fixtures.BasicPages.Pages_Internal_Page_Elm as Pages_Internal_Page_Elm
import Spa.Fixtures.BasicPages.Pages_Internal_Router_Elm as Pages_Internal_Router_Elm
import Spa.Fixtures.BasicPagesWithParams as BasicPagesWithParams
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
        [ test "fail if routes are not satisfied" <|
            \_ ->
                Spa.initDecodingConfig configSimple
                    |> Spa.addPage BasicPages.about
                    |> Spa.addPage BasicPages.counter
                    |> Spa.build
                    |> Expect.err
        , basicSuite "simple pages"
            configSimple
            BasicPages.pages
            { main_elm = Main_Elm.content
            , pages_elm = Pages_Elm.contentSimple
            , pages_internal_router_elm = Pages_Internal_Router_Elm.contentSimple
            , pages_internal_page_elm = Pages_Internal_Page_Elm.contentSimple
            }
        , basicSuite "with route params"
            configWithRouteParams
            BasicPagesWithParams.pages
            { main_elm = Main_Elm.content
            , pages_elm = Pages_Elm.contentWithRouteParams
            , pages_internal_router_elm = Pages_Internal_Router_Elm.contentWithRouteParams
            , pages_internal_page_elm = Pages_Internal_Page_Elm.contentWithRouteParams
            }
        ]


basicSuite :
    String
    -> Json.Value
    ->
        { index : String
        , about : String
        , counter : String
        , counterAsync : String
        }
    ->
        { main_elm : String
        , pages_elm : String
        , pages_internal_router_elm : String
        , pages_internal_page_elm : String
        }
    -> Test
basicSuite name config pages content =
    describe name
        [ test "should generate the App/Main.elm file" <|
            \_ ->
                justGetFile config pages "App/Main.elm"
                    |> Expect.equal (Ok content.main_elm)
        , test "should generate the App/Pages.elm file" <|
            \_ ->
                justGetFile config pages "App/Pages.elm"
                    |> Expect.equal (Ok content.pages_elm)
        , test "should generate the App/Pages/Internal/Router.elm file" <|
            \_ ->
                justGetFile config pages "App/Pages/Internal/Router.elm"
                    |> Expect.equal (Ok content.pages_internal_router_elm)
        , test "should generate the App/Pages/Internal/Page.elm file" <|
            \_ ->
                justGetFile config pages "App/Pages/Internal/Page.elm"
                    |> Expect.equal (Ok content.pages_internal_page_elm)
        ]


context :
    Json.Value
    ->
        { index : String
        , about : String
        , counter : String
        , counterAsync : String
        }
    -> Result Error Context
context config pages =
    Spa.initDecodingConfig config
        |> Spa.addPage pages.about
        |> Spa.addPage pages.counter
        |> Spa.addPage pages.counterAsync
        |> Spa.addPage pages.index
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


justGetFile :
    Json.Value
    ->
        { index : String
        , about : String
        , counter : String
        , counterAsync : String
        }
    -> String
    -> Result Error String
justGetFile config pages name =
    context config pages
        |> result
        |> getFile name
