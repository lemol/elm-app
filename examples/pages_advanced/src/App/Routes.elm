module App.Routes exposing (Route(..), parseUrl, toPath)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = About
    | Counter
    | CounterAsync
    | Index
    | NotFound


parseUrl : Url -> Route
parseUrl url =
    let
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
        prefix =
            "#"

        path =
            case route of
                About ->
                    "about"

                Counter ->
                    "counter"

                CounterAsync ->
                    "counterasync"

                Index ->
                    "/"

                NotFound ->
                    "/404"
    in
    prefix ++ path


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map About (s "about")
        , map Counter (s "counter")
        , map CounterAsync (s "counterasync")
        , map Index top
        , map NotFound (s "/404")
        ]
