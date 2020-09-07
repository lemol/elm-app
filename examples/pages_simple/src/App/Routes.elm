module App.Routes exposing (Route(..), parseUrl, toPath)

import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = Index
    | About
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
                NotFound ->
                    "/404"

                Index ->
                    "/"

                About ->
                    "/about"
    in
    prefix ++ path


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Index top
        , map About (s "about")
        ]
