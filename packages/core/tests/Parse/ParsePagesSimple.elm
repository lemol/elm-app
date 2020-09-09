module Parse.ParsePagesSimple exposing (..)

import Expect
import Parser
import Test exposing (..)



-- suite : Test
-- suite =
--     describe "Multiple pages"
--         [ test "with two simple Html msg pages" <|
--             \_ ->
--                 let
--                     result =
--                         Ok EmptyApp
--                             |> Result.andThen (\app -> Parser.parseNextSource MulitplePagesAppType app indexPage)
--                             |> Result.andThen (\app -> Parser.parseNextSource MulitplePagesAppType app aboutPage)
--                     expected =
--                         MulitplePagesApp
--                             [ { routeName = "Index"
--                               , routePath = "/"
--                               , options =
--                                     { moduleName = "Pages.Index"
--                                     , initType = Init0
--                                     , mainType = Main0
--                                     , updateType = Update0
--                                     , viewType = View1
--                                     , subscriptionType = Subscription0
--                                     }
--                               }
--                             , { routeName = "About"
--                               , routePath = "/about"
--                               , options =
--                                     { moduleName = "Pages.About"
--                                     , initType = Init0
--                                     , mainType = Main0
--                                     , updateType = Update0
--                                     , viewType = View1
--                                     , subscriptionType = Subscription0
--                                     }
--                               }
--                             ]
--                 in
--                 result |> Expect.equal (Ok expected)
--         ]


indexPage : String
indexPage =
    """
module Pages.Index exposing (view)

import App.Routes as Routes exposing (toPath)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href)


view : Html msg
view =
    div
        []
        [ h1
            []
            [ text "Index Page" ]
        , div
            []
            [ a
                [ href (toPath Routes.Index) ]
                [ text "Index" ]
            , text " | "
            , a
                [ href (toPath Routes.About) ]
                [ text "About" ]
            ]
        ]
"""


aboutPage : String
aboutPage =
    """
module Pages.About exposing (view)

import App.Routes as Routes exposing (toPath)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href)


view : Html msg
view =
    div
        []
        [ h1
            []
            [ text "About Page" ]
        , div
            []
            [ a
                [ href (toPath Routes.Index) ]
                [ text "Index" ]
            , text " | "
            , a
                [ href (toPath Routes.About) ]
                [ text "About" ]
            ]
        ]
  """
