module App.Utils exposing (mapDocument, send)

import Browser exposing (Document)
import Html
import Task



-- BASIC


send : msg -> Cmd msg
send =
    Task.succeed >> Task.perform identity



-- HTML


mapDocument : (msg1 -> msg2) -> Document msg1 -> Document msg2
mapDocument f { title, body } =
    { title = title
    , body =
        List.map (Html.map f) body
    }
