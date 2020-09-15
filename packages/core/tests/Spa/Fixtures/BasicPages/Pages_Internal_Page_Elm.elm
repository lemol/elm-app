module Spa.Fixtures.BasicPages.Pages_Internal_Page_Elm exposing (..)


contentSimple : String
contentSimple =
    """module App.Pages.Internal.Page exposing (PageLoaded(..))

import Pages.About
import Pages.Counter
import Pages.CounterAsync
import Pages.Index


type PageLoaded
    = None
    | Index () ()
    | About () ()
    | Counter Pages.Counter.Model ()
    | CounterAsync Pages.CounterAsync.Model ()
"""


contentWithRouteParams : String
contentWithRouteParams =
    """module App.Pages.Internal.Page exposing (PageLoaded(..))

import Pages.About
import Pages.Counter
import Pages.CounterAsync
import Pages.Index


type PageLoaded
    = None
    | Index () ()
    | About () Pages.About.Params
    | Counter Pages.Counter.Model ()
    | CounterAsync Pages.CounterAsync.Model Pages.CounterAsync.Params
"""
