module Pages.Index exposing (view)

import App.Routes as Routes exposing (toPath)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href)


view : Html msg
view =
    div
        []
        [ h1
            []
            [ text "Index Page" ]
        , div
            []
            [ a
                [ href (toPath Routes.Index) ]
                [ text "Index" ]
            , text " | "
            , a
                [ href (toPath Routes.About) ]
                [ text "About" ]
            ]
        ]
