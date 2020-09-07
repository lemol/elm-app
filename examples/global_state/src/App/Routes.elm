module App.Routes exposing (Route(..), parseUrl, toPath)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)



-- DATA


type Route
    = Index
    | About
    | Counter
    | CounterAsync
    | NotFound


parseUrl : Url -> Route
parseUrl url =
    let
        -- for HashRouting { url | path = url.fragment |> Maybe.withDefault "" }
        newUrl =
            { url | path = url.fragment |> Maybe.withDefault "" }
    in
    case parse matchRoute newUrl of
        Just route ->
            route

        Nothing ->
            NotFound


toPath : Route -> String
toPath route =
    let
        -- for HashRouting #
        prefix =
            "#"

        path =
            case route of
                NotFound ->
                    "/404"

                Index ->
                    "/"

                About ->
                    "/about"

                Counter ->
                    "/counter"

                CounterAsync ->
                    "/counter-async"
    in
    prefix ++ path


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Index top
        , map About (s "about")
        , map Counter (s "counter")
        , map CounterAsync (s "counter-async")
        ]
