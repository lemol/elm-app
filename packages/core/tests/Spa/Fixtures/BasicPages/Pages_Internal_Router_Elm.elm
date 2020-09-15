module Spa.Fixtures.BasicPages.Pages_Internal_Router_Elm exposing (..)


contentSimple : String
contentSimple =
    """module App.Pages.Internal.Router exposing (AppUrl, AppUrls, Model, Route(..), init, parseUrl, toPath, urlKey, urlRoute, urls)

import Browser.Navigation as Navigation
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type alias Model =
    { navigationKey : Navigation.Key, route : Route }


init : Navigation.Key -> Route -> Model
init key route =
    { navigationKey = key, route = route }


type Route
    = Index
    | About
    | Counter
    | CounterAsync
    | NotFound


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


toPath : Route -> String
toPath route =
    case route of
        Index ->
            "/"

        About ->
            "/" ++ "about"

        Counter ->
            "/" ++ "counter"

        CounterAsync ->
            "/" ++ "counter" ++ "/" ++ "async"

        NotFound ->
            "/" ++ "404"


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Index top
        , map About (s "about")
        , map Counter (s "counter")
        , map CounterAsync (s "counter" </> s "async")
        , map NotFound (s "404")
        ]


type AppUrl
    = AppUrl Navigation.Key Route


type alias AppUrls =
    { index : AppUrl, about : AppUrl, counter : AppUrl, counterAsync : AppUrl }


urls : Navigation.Key -> AppUrls
urls navigationKey =
    { index = AppUrl navigationKey Index
    , about = AppUrl navigationKey About
    , counter = AppUrl navigationKey Counter
    , counterAsync = AppUrl navigationKey CounterAsync
    }


urlKey : AppUrl -> Navigation.Key
urlKey (AppUrl key _) =
    key


urlRoute : AppUrl -> Route
urlRoute (AppUrl _ route) =
    route
"""


contentWithRouteParams : String
contentWithRouteParams =
    """module App.Pages.Internal.Router exposing (AppUrl, AppUrls, Model, Route(..), init, parseUrl, toPath, urlKey, urlRoute, urls)

import Browser.Navigation as Navigation
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


type alias Model =
    { navigationKey : Navigation.Key, route : Route }


init : Navigation.Key -> Route -> Model
init key route =
    { navigationKey = key, route = route }


type Route
    = Index
    | About String String
    | Counter
    | CounterAsync String Int Int
    | NotFound


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


toPath : Route -> String
toPath route =
    case route of
        Index ->
            "/"

        About countryId userId ->
            "/" ++ "about" ++ "/" ++ countryId ++ "/" ++ "users" ++ "/" ++ userId

        Counter ->
            "/" ++ "counter"

        CounterAsync name initial step ->
            "/"
                ++ "counter"
                ++ "/"
                ++ "async"
                ++ "/"
                ++ name
                ++ "/"
                ++ String.fromInt initial
                ++ "/"
                ++ String.fromInt step
                ++ "/"
                ++ "go"

        NotFound ->
            "/" ++ "404"


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Index top
        , map About (s "about" </> string </> s "users" </> string)
        , map Counter (s "counter")
        , map CounterAsync (s "counter" </> s "async" </> string </> int </> int </> s "go")
        , map NotFound (s "404")
        ]


type AppUrl
    = AppUrl Navigation.Key Route


type alias AppUrls =
    { index : AppUrl
    , about : String -> String -> AppUrl
    , counter : AppUrl
    , counterAsync : String -> Int -> Int -> AppUrl
    }


urls : Navigation.Key -> AppUrls
urls navigationKey =
    { index = AppUrl navigationKey Index
    , about = \\countryId userId -> AppUrl navigationKey (About countryId userId)
    , counter = AppUrl navigationKey Counter
    , counterAsync = \\name initial step -> AppUrl navigationKey (CounterAsync name initial step)
    }


urlKey : AppUrl -> Navigation.Key
urlKey (AppUrl key _) =
    key


urlRoute : AppUrl -> Route
urlRoute (AppUrl _ route) =
    route
"""
