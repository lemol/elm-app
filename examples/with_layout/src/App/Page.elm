module App.Page exposing (Model, Msg, enterRoute, init, subscriptions, update, view)

import App.Routes as Routes
import App.Utils as Utils exposing (mapDocument)
import Browser
import Global
import Html exposing (Html, text)
import Layouts.Main
import Pages.About
import Pages.Counter
import Pages.CounterAsync
import Pages.Index



-- MODEL


type alias Model =
    { mainLayout : Maybe Layouts.Main.Model
    , index : Maybe ()
    , about : Maybe ()
    , counter : Maybe Pages.Counter.Model
    , counterAsync : Maybe Pages.CounterAsync.Model
    }


init : Global.Model -> Routes.Route -> ( Model, Cmd Msg )
init global route =
    let
        model =
            { mainLayout = Nothing
            , index = Nothing
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

                ( layoutModel, layoutCmd ) =
                    Layouts.Main.init
            in
            ( { model
                | counterAsync =
                    model.counterAsync
                        |> Maybe.withDefault pageModel
                        |> Just
                , mainLayout =
                    model.mainLayout
                        |> Maybe.withDefault layoutModel
                        |> Just
              }
            , Cmd.batch
                [ Cmd.map MainLayoutMsg layoutCmd
                , Cmd.map CounterAsyncMsg pageCmd
                ]
            )



-- MESSAGES


type Msg
    = MainLayoutMsg Layouts.Main.Msg
    | IndexMsg ()
    | AboutMsg ()
    | CounterMsg Pages.Counter.Msg
    | Counter_LayoutMsg (Utils.LayoutMsg Layouts.Main.Msg Pages.Counter.Msg)
    | CounterAsyncMsg Pages.CounterAsync.Msg
    | CounterAsync_LayoutMsg (Utils.LayoutMsg Layouts.Main.Msg Pages.CounterAsync.Msg)



-- UPDATE


update : Msg -> Global.Model -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg global model =
    case msg of
        MainLayoutMsg subMsg ->
            let
                ( pageModel, pageCmd, globalCmd ) =
                    updateHelper3 Layouts.Main.update subMsg model.mainLayout
            in
            ( { model | mainLayout = pageModel }
            , Cmd.map MainLayoutMsg pageCmd
            , globalCmd
            )

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

        Counter_LayoutMsg subMsg ->
            let
                split =
                    Utils.splitMsg MainLayoutMsg CounterMsg
            in
            update (split subMsg) global model

        CounterAsyncMsg subMsg ->
            let
                ( pageModel, pageCmd, globalCmd ) =
                    updateHelper3 Pages.CounterAsync.update subMsg model.counterAsync
            in
            ( { model | counterAsync = pageModel }
            , Cmd.map CounterAsyncMsg pageCmd
            , globalCmd
            )

        CounterAsync_LayoutMsg subMsg ->
            let
                split =
                    Utils.splitMsg MainLayoutMsg CounterAsyncMsg
            in
            update (split subMsg) global model



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
            case ( model.counter, model.mainLayout ) of
                ( Just counter, Just mainLayout ) ->
                    Layouts.Main.view global mainLayout (Pages.Counter.view global counter)
                        |> mapDocument Counter_LayoutMsg

                _ ->
                    viewEmpty

        Routes.CounterAsync ->
            case ( model.counterAsync, model.mainLayout ) of
                ( Just counterAsync, Just mainLayout ) ->
                    Layouts.Main.view global mainLayout (Pages.CounterAsync.view global counterAsync)
                        |> mapDocument CounterAsync_LayoutMsg

                _ ->
                    viewEmpty


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
