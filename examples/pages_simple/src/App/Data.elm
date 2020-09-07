module App.Data exposing (Model, init)

import App.Routes as Routes
import Browser.Navigation as Navigation


type alias Model =
    { navigationKey : Navigation.Key
    , route : Routes.Route
    }


init : Navigation.Key -> Routes.Route -> Model
init key route =
    { navigationKey = key
    , route = route
    }
