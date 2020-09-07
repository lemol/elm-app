module App.Utils exposing (mapDocument, dispatch, updateHelper1, updateHelper2)

import Browser exposing (Document)
import Html
import Task



-- BASIC


dispatch : msg -> Cmd msg
dispatch =
    Task.succeed >> Task.perform identity



-- HTML


mapDocument : (msg1 -> msg2) -> Document msg1 -> Document msg2
mapDocument f { title, body } =
    { title = title
    , body =
        List.map (Html.map f) body
    }



-- UPDATE


updateHelper1 : (msg -> model -> model) -> msg -> Maybe model -> Maybe model
updateHelper1 f msg =
    Maybe.map (f msg)


updateHelper2 : (msg -> model -> ( model, Cmd msg )) -> msg -> Maybe model -> ( Maybe model, Cmd msg )
updateHelper2 f msg maybeModel =
    case maybeModel of
        Nothing ->
            ( Nothing, Cmd.none )

        Just model ->
            f msg model
                |> Tuple.mapFirst Just
  