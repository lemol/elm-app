module Spa.Fixtures.MainModule exposing (..)


file : String
file =
    """module App.Main exposing (main)

import App.Pages.Document as Document
import App.Pages.Internal.Router as Router exposing (parseUrl)
import App.Pages.Main as Page
import Browser
import Browser.Navigation as Navigation
import Url


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
    { router : Router.Model
    , page : Page.Model
    }


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
            Router.init key route

        bag =
            { router = appModel
            }

        ( pageModel, pageCmd ) =
            Page.init bag route

        model =
            { router = appModel
            , page = pageModel
            }
    in
    ( model
    , Cmd.batch
        [ Cmd.map GlobalMsg globalCmd
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl
                        model.router.navigationKey
                        (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChanged url ->
            let
                router =
                    model.router

                route =
                    parseUrl url

                newRouter =
                    { router | route = route }

                bag =
                    { router = newApp
                    }

                ( newPage, newPageCmd ) =
                    Page.enterRoute bag model.page route
            in
            ( { model | page = newPage, router = newRouter }
            , Cmd.batch [ Cmd.map PageMsg newPageCmd ]
            )

        PageMsg subMsg ->
            let
                bag =
                    { global = model.global
                    , router = model.router
                    }

                ( newPage, newPageCmd ) =
                    Page.update bag subMsg model.page
            in
            ( { model | page = newPage }
            , Cmd.batch
                [ Cmd.map PageMsg newPageCmd
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map PageMsg (Page.subscriptions model.page)
        ]


view : Model -> Browser.Document Msg
view model =
    Page.view { global = model.global, router = model.router } model.page
        |> Document.map PageMsg
        |> Document.toBrowserDocument

    """
