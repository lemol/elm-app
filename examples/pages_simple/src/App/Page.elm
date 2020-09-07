module App.Page exposing (Model, Msg, enterRoute, init, update, view)

import App.NotFound
import App.Routes as Routes
import App.Utils exposing (mapDocument)
import Browser
import Html exposing (Html, text)
import Pages.About
import Pages.Index



-- MODEL


type alias Model =
    { index : Maybe ()
    , about : Maybe ()
    }


init : Routes.Route -> ( Model, Cmd Msg )
init route =
    let
        model =
            { index = Nothing
            , about = Nothing
            }
    in
    enterRoute model route


enterRoute : Model -> Routes.Route -> ( Model, Cmd Msg )
enterRoute model route =
    case route of
        Routes.NotFound ->
            ( model
            , Cmd.none
            )

        Routes.Index ->
            ( { model
                | index =
                    model.index
                        |> Maybe.withDefault ()
                        |> Just
              }
            , Cmd.none
            )

        Routes.About ->
            ( { model
                | about =
                    model.about
                        |> Maybe.withDefault ()
                        |> Just
              }
            , Cmd.none
            )



-- MESSAGES


type Msg
    = IndexMsg ()
    | AboutMsg ()



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IndexMsg _ ->
            ( model, Cmd.none )

        AboutMsg _ ->
            ( model, Cmd.none )



-- VIEW


view : Routes.Route -> Model -> Browser.Document Msg
view route model =
    case route of
        Routes.NotFound ->
            App.NotFound.view
                |> toDocument "404"

        Routes.Index ->
            model.index
                |> Maybe.map (always Pages.Index.view)
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument (always (IndexMsg ()))

        Routes.About ->
            model.about
                |> Maybe.map (always Pages.About.view)
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument (always (AboutMsg ()))


viewEmpty : Browser.Document msg
viewEmpty =
    { title = ""
    , body = [ text "" ]
    }


toDocument : String -> Html msg -> Browser.Document msg
toDocument title body =
    { title = title
    , body = [ body ]
    }
