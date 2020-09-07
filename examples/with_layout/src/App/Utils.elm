module App.Utils exposing (RoutingType(..), mapDocument, LayoutMsg(..), send, splitMsg)

import Browser exposing (Document)
import Html
import Task



-- BASIC


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity



-- ROUTING


type RoutingType
    = HashRouting
    | BrowserRouting



-- HTML


mapDocument : (msg1 -> msg2) -> Document msg1 -> Document msg2
mapDocument f { title, body } =
    { title = title
    , body =
        List.map (Html.map f) body
    }



-- LAYOUT


type LayoutMsg baseMsg pageMsg
    = BaseMsg baseMsg
    | PageMsg pageMsg


splitMsg : (baseMsg -> msg) -> (pageMsg -> msg) -> LayoutMsg baseMsg pageMsg -> msg
splitMsg fromBaseMsg fromPageMsg layoutMsg =
    case layoutMsg of
        BaseMsg x ->
            fromBaseMsg x

        PageMsg x ->
            fromPageMsg x