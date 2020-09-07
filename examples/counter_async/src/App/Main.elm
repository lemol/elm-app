module App.Main exposing (main)

import Browser
import Main exposing (Model, Msg, init, subscriptions, update, view)


main : Program () Model Msg
main =
    Browser.element
        { init = always init, update = update, view = view, subscriptions = subscriptions }
