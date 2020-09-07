module App.Page exposing (Model, Msg, enterRoute, init, subscriptions, update, view)

import App.Routes as Routes
import App.Utils exposing (mapDocument)
import Browser
import Global
import Html exposing (Html, text)
import Pages.About
import Pages.Counter
import Pages.CounterAsync
import Pages.Index



-- MODEL


type alias Model =
    { index : Maybe ()
    , about : Maybe ()
    , counter : Maybe Pages.Counter.Model
    , counterAsync : Maybe Pages.CounterAsync.Model
    }


init : Global.Model -> Routes.Route -> ( Model, Cmd Msg )
init global route =
    let
        model =
            { index = Nothing
            , about = Nothing
            , counter = Nothing
            , counterAsync = Nothing
            }
    in
    enterRoute global model route


enterRoute : Global.Model -> Model -> Routes.Route -> ( Model, Cmd Msg )
enterRoute global model route =
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

        Routes.Counter ->
            let
                pageModel =
                    Pages.Counter.init
            in
            ( { model
                | counter =
                    model.counter
                        |> Maybe.withDefault pageModel
                        |> Just
              }
            , Cmd.none
            )

        Routes.CounterAsync ->
            let
                ( pageModel, pageCmd ) =
                    Pages.CounterAsync.init
            in
            ( { model
                | counterAsync =
                    model.counterAsync
                        |> Maybe.withDefault pageModel
                        |> Just
              }
            , Cmd.map CounterAsyncMsg pageCmd
            )



-- MESSAGES


type Msg
    = IndexMsg ()
    | AboutMsg ()
    | CounterMsg Pages.Counter.Msg
    | CounterAsyncMsg Pages.CounterAsync.Msg



-- UPDATE


update : Msg -> Global.Model -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg _ model =
    case msg of
        IndexMsg _ ->
            ( model, Cmd.none, Cmd.none )

        AboutMsg _ ->
            ( model, Cmd.none, Cmd.none )

        CounterMsg subMsg ->
            let
                ( pageModel, pageCmd, globalCmd ) =
                    updateHelper3 Pages.Counter.update subMsg model.counter
            in
            ( { model | counter = pageModel }
            , Cmd.map CounterMsg pageCmd
            , globalCmd
            )

        CounterAsyncMsg subMsg ->
            let
                ( pageModel, pageCmd, globalCmd ) =
                    updateHelper3 Pages.CounterAsync.update subMsg model.counterAsync
            in
            ( { model | counterAsync = pageModel }
            , Cmd.map CounterAsyncMsg pageCmd
            , globalCmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ model.counterAsync
        |> Maybe.map Pages.CounterAsync.subscriptions
        |> Maybe.map (Sub.map CounterAsyncMsg)
    ]
        |> List.filterMap identity
        |> Sub.batch



-- VIEW


view : Routes.Route -> Global.Model -> Model -> Browser.Document Msg
view route global model =
    case route of
        Routes.NotFound ->
            { title = "404"
            , body = [ text "Not Found" ]
            }

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

        Routes.Counter ->
            model.counter
                |> Maybe.map (Pages.Counter.view global)
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument CounterMsg

        Routes.CounterAsync ->
            model.counterAsync
                |> Maybe.map (Pages.CounterAsync.view global)
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument CounterAsyncMsg


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



-- UTILS


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


updateHelper3 : (msg -> model -> ( model, Cmd msg, Cmd globalMsg )) -> msg -> Maybe model -> ( Maybe model, Cmd msg, Cmd globalMsg )
updateHelper3 f msg maybeModel =
    case maybeModel of
        Nothing ->
            ( Nothing, Cmd.none, Cmd.none )

        Just model ->
            case f msg model of
                ( x, y, z ) ->
                    ( Just x, y, z )
