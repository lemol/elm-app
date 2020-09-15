module Spa.Fixtures.BasicPages.Pages_Internal_Page exposing (..)


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
