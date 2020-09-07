module App.Page exposing (Model, Msg(..), enterRoute, init, subscriptions, update, view)

import App.NotFound
import App.Routes as Routes
import App.Utils exposing (mapDocument, updateHelper1, updateHelper2)
import Browser
import Html exposing (Html, text)
import Pages.About
import Pages.Counter
import Pages.CounterAsync
import Pages.Index


type alias Model =
    { counter : Maybe Pages.Counter.Model, counterAsync : Maybe Pages.CounterAsync.Model }


init : Routes.Route -> ( Model, Cmd Msg )
init route =
    let
        model =
            { counter = Nothing, counterAsync = Nothing }
    in
    enterRoute model route


enterRoute : Model -> Routes.Route -> ( Model, Cmd Msg )
enterRoute model route =
    case route of
        Routes.Counter ->
            let
                pageModel =
                    Pages.Counter.init
            in
            ( { model | counter = model.counter |> Maybe.withDefault pageModel |> Just }, Cmd.none )

        Routes.CounterAsync ->
            let
                ( pageModel, pageCmd ) =
                    Pages.CounterAsync.init
            in
            ( { model | counterAsync = model.counterAsync |> Maybe.withDefault pageModel |> Just }
            , Cmd.map CounterAsyncMsg pageCmd
            )

        _ ->
            ( model, Cmd.none )


type Msg
    = CounterMsg Pages.Counter.Msg
    | CounterAsyncMsg Pages.CounterAsync.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg subMsg ->
            let
                pageModel =
                    updateHelper1 Pages.Counter.update subMsg model.counter
            in
            ( { model | counter = pageModel }, Cmd.none )

        CounterAsyncMsg subMsg ->
            let
                ( pageModel, pageCmd ) =
                    updateHelper2 Pages.CounterAsync.update subMsg model.counterAsync
            in
            ( { model | counterAsync = pageModel }, Cmd.map CounterAsyncMsg pageCmd )


view : Routes.Route -> Model -> Browser.Document Msg
view route model =
    case route of
        Routes.About ->
            Pages.About.view |> toDocument ""

        Routes.Counter ->
            model.counter
                |> Maybe.map Pages.Counter.view
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument CounterMsg

        Routes.CounterAsync ->
            model.counterAsync
                |> Maybe.map Pages.CounterAsync.view
                |> Maybe.map (toDocument "")
                |> Maybe.withDefault viewEmpty
                |> mapDocument CounterAsyncMsg

        Routes.Index ->
            Pages.Index.view |> toDocument ""

        Routes.NotFound ->
            App.NotFound.view |> toDocument ""


viewEmpty : Browser.Document msg
viewEmpty =
    { title = "", body = [ text "Page not loaded" ] }


toDocument : String -> Html msg -> Browser.Document msg
toDocument title body =
    { title = title, body = [ body ] }


subscriptions : Model -> Sub Msg
subscriptions model =
    [ model.counterAsync
        |> Maybe.map Pages.CounterAsync.subscriptions
        |> Maybe.map (Sub.map CounterAsyncMsg)
    ]
        |> List.filterMap identity
        |> Sub.batch
