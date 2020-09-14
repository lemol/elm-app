module Spa.Fixtures.BasicPages.PagesModule exposing (..)


contentSimple : String
contentSimple =
    """module App.Pages exposing (AppUrl, AppUrls, Page(..), async, fromRoute, pushUrl, urlPage, urlString)

import App.Pages.Internal.Router as Router
import Browser.Navigation as Navigation
import Task


async : msg -> Cmd msg
async =
    Task.succeed >> Task.perform identity


pushUrl : AppUrl -> Cmd msg
pushUrl url =
    Navigation.pushUrl (Router.urlKey url) (Router.toPath (Router.urlRoute url))


type Page
    = Index
    | About
    | Counter
    | CounterAsync


fromRoute : Router.Route -> Page
fromRoute route =
    case route of
        Router.Index ->
            Index

        Router.About ->
            About

        Router.Counter ->
            Counter

        Router.CounterAsync ->
            CounterAsync


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


contentWithRouteParams : String
contentWithRouteParams =
    """module App.Pages exposing (AppUrl, AppUrls, Page(..), async, fromRoute, pushUrl, urlPage, urlString)

import App.Pages.Internal.Router as Router
import Browser.Navigation as Navigation
import Task


async : msg -> Cmd msg
async =
    Task.succeed >> Task.perform identity


pushUrl : AppUrl -> Cmd msg
pushUrl url =
    Navigation.pushUrl (Router.urlKey url) (Router.toPath (Router.urlRoute url))


type Page
    = Index
    | About
    | Counter
    | CounterAsync


fromRoute : Router.Route -> Page
fromRoute route =
    case route of
        Router.Index ->
            Index

        Router.About _ _ ->
            About

        Router.Counter ->
            Counter

        Router.CounterAsync _ _ _ ->
            CounterAsync


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
