module App.Main exposing (main)

import App.Data as Data
import App.Page as Page
import App.Routes exposing (parseUrl)
import App.Utils exposing (mapDocument)
import Browser
import Browser.Navigation as Navigation
import Url
import Url.Parser exposing (map)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Flags =
    ()


type alias Model =
    { app : Data.Model, page : Page.Model }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PageMsg Page.Msg


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            parseUrl url

        appModel =
            Data.init key route

        ( pageModel, pageCmd ) =
            Page.init route

        model =
            { app = appModel, page = pageModel }
    in
    ( model, Cmd.batch [ Cmd.map PageMsg pageCmd ] )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.app.navigationKey (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        UrlChanged url ->
            let
                app =
                    model.app

                route =
                    parseUrl url

                newApp =
                    { app | route = route }

                ( newPage, newPageCmd ) =
                    Page.enterRoute model.page route
            in
            ( { model | page = newPage, app = newApp }, Cmd.batch [ Cmd.map PageMsg newPageCmd ] )

        PageMsg subMsg ->
            let
                ( newPage, newPageCmd ) =
                    Page.update subMsg model.page
            in
            ( { model | page = newPage }, Cmd.batch [ Cmd.map PageMsg newPageCmd ] )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map PageMsg (Page.subscriptions model.page) ]


view : Model -> Browser.Document Msg
view model =
    Page.view model.app.route model.page |> mapDocument PageMsg
