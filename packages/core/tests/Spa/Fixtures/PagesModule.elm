module Spa.Fixtures.PagesModule exposing (..)


content : String
content =
    """module App.Pages exposing (AppUrl, AppUrls, Page(..), async, fromRoute, pushUrl, urlPage, urlString)

import App.Pages.Internal.Router as Router
import Browser.Navigation as Navigation
import Task


async : msg -> Cmd msg
async =
    Task.succeed >> Task.perform identity


pushUrl : AppUrl -> Cmd msg
pushUrl url =
    Navigation.pushUrl
        (Router.urlKey url)
        (Router.toPath (Router.urlRoute url))


type Page
    = About
    | Counter
    | CounterAsync
    | Index


fromRoute : Router.Route -> Page
fromRoute route =
    case route of
        Router.About ->
            About

        Router.Counter ->
            Counter

        Router.CounterAsync ->
            CounterAsync

        Router.Index ->
            Index


type alias AppUrl =
    Router.AppUrl


type alias AppUrls =
    Router.AppUrls


urlPage : AppUrl -> Page
urlPage =
    Router.urlRoute >> fromRoute


urlString : AppUrl -> String
urlString =
    Router.urlRoute >> Router.toPath
"""
