module Parse.ParsePagesAdvanced exposing (..)

import ElmApp.Parser
import Expect
import Test exposing (..)



-- suite : Test
-- suite =
--     describe "Multiple Pages Advanced"
--         [ test "with advanced pages" <|
--             \_ ->
--                 let
--                     result =
--                         Ok EmptyApp
--                             |> Result.andThen (\app -> Parser.parseNextSource MulitplePagesAppType app indexPage)
--                             |> Result.andThen (\app -> Parser.parseNextSource MulitplePagesAppType app aboutPage)
--                             |> Result.andThen (\app -> Parser.parseNextSource MulitplePagesAppType app counterPage)
--                             |> Result.andThen (\app -> Parser.parseNextSource MulitplePagesAppType app counterAsyncPage)
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
--                             , { routeName = "Counter"
--                               , routePath = "/counter"
--                               , options =
--                                     { moduleName = "Pages.Counter"
--                                     , initType = Init1
--                                     , mainType = Main0
--                                     , updateType = Update3
--                                     , viewType = View2
--                                     , subscriptionType = Subscription0
--                                     }
--                               }
--                             , { routeName = "CounterAsync"
--                               , routePath = "/counterasync"
--                               , options =
--                                     { moduleName = "Pages.CounterAsync"
--                                     , initType = Init2
--                                     , mainType = Main0
--                                     , updateType = Update4
--                                     , viewType = View2
--                                     , subscriptionType = Subscription2
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
                [ href (toPath Routes.Counter) ]
                [ text "Counter" ]
            , text " | "
            , a
                [ href (toPath Routes.CounterAsync) ]
                [ text "Counter Async" ]
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
                [ href (toPath Routes.Counter) ]
                [ text "Counter" ]
            , text " | "
            , a
                [ href (toPath Routes.CounterAsync) ]
                [ text "Counter Async" ]
            , text " | "
            , a
                [ href (toPath Routes.About) ]
                [ text "About" ]
            ]
        ]
  """


counterPage : String
counterPage =
    """
module Pages.Counter exposing (Model, Msg, init, update, view)

import App.Routes as Routes exposing (toPath)
import Html exposing (Html, a, button, div, h1, text)
import Html.Attributes exposing (href)
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
        [ h1
            []
            [ text "Counter Page" ]
        , div
            []
            [ a
                [ href (toPath Routes.Index) ]
                [ text "Index" ]
            , text " | "
            , a
                [ href (toPath Routes.Counter) ]
                [ text "Counter" ]
            , text " | "
            , a
                [ href (toPath Routes.CounterAsync) ]
                [ text "Counter Async" ]
            , text " | "
            , a
                [ href (toPath Routes.About) ]
                [ text "About" ]
            ]
        , div
            []
            [ button
                [ onClick Decrement ]
                [ text "-" ]
            , text (String.fromInt model)
            , button
                [ onClick Increment ]
                [ text "+" ]
            ]
        ]
    """


counterAsyncPage : String
counterAsyncPage =
    """
module Pages.CounterAsync exposing (Model, Msg, init, subscriptions, update, view)

import App.Routes as Routes exposing (toPath)
import Html exposing (Html, a, button, div, h1, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Time



-- MODEL


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0
    , Cmd.none
    )



-- MESSAGES


type Msg
    = Increment
    | Reset



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1
            , Cmd.none
            )

        Reset ->
            ( 0
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (always Increment)



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ h1
            []
            [ text "Counter Async Page" ]
        , div
            []
            [ a
                [ href (toPath Routes.Index) ]
                [ text "Index" ]
            , text " | "
            , a
                [ href (toPath Routes.Counter) ]
                [ text "Counter" ]
            , text " | "
            , a
                [ href (toPath Routes.CounterAsync) ]
                [ text "Counter Async" ]
            , text " | "
            , a
                [ href (toPath Routes.About) ]
                [ text "About" ]
            ]
        , div
            []
            [ text (String.fromInt model)
            , button
                [ onClick Reset
                ]
                [ text "reset"
                ]
            ]
        ]
    """
