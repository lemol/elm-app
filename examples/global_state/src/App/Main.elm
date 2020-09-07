module App.Main exposing (main)

import App.Data as App
import App.Page as Page
import App.Routes exposing (parseUrl)
import App.Utils exposing (mapDocument)
import Browser
import Browser.Navigation as Navigation
import Global
import Url
import Url.Parser exposing (map)



-- PROGRAM


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



-- DATA


type alias Flags =
    ()



-- MODEL


type alias Model =
    { app : App.Model
    , global : Global.Model
    , page : Page.Model
    }



-- MESSAGES


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GlobalMsg Global.Msg
    | PageMsg Page.Msg


init : Flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            parseUrl url

        app =
            App.init key route

        ( globalModel, globalCmd ) =
            Global.init

        ( pageModel, pageCmd ) =
            Page.init globalModel route

        model =
            { app = app
            , global = globalModel
            , page = pageModel
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map GlobalMsg globalCmd
        , Cmd.map PageMsg pageCmd
        ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.app.navigationKey (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChanged url ->
            let
                app =
                    model.app

                route =
                    parseUrl url

                newApp =
                    { app | route = route }

                ( newPage, newPageCmd ) =
                    Page.enterRoute model.global model.page route
            in
            ( { model
                | page = newPage
                , app = newApp
              }
            , Cmd.batch
                [ Cmd.map PageMsg newPageCmd
                ]
            )

        GlobalMsg subMsg ->
            let
                ( newGlobal, newGlobalCmd ) =
                    Global.update subMsg model.global
            in
            ( { model | global = newGlobal }
            , Cmd.batch
                [ Cmd.map GlobalMsg newGlobalCmd
                ]
            )

        PageMsg subMsg ->
            let
                ( newPage, newPageCmd, newGlobalCmd ) =
                    Page.update subMsg model.global model.page
            in
            ( { model | page = newPage }
            , Cmd.batch
                [ Cmd.map GlobalMsg newGlobalCmd
                , Cmd.map PageMsg newPageCmd
                ]
            )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Page.subscriptions model.page
            |> Sub.map PageMsg
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Page.view model.app.route model.global model.page
        |> mapDocument PageMsg
