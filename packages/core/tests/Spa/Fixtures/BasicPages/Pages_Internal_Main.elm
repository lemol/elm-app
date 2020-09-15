module Spa.Fixtures.BasicPages.Pages_Internal_Main exposing (..)


contentSimple : String
contentSimple =
    """module App.Pages.Internal.Main exposing (ExternalMsg(..), Model, Msg(..), enterRoute, init, subscriptions, update, view)

import App.Pages
import App.Pages.Document exposing (Document)
import App.Pages.Internal.Page as Page exposing (PageLoaded(..))
import App.Pages.Internal.Router as Router
import Browser.Navigation as Navigation
import Pages.About
import Pages.Counter
import Pages.CounterAsync
import Pages.Index


type alias Bag =
    { router : Router.Model
    }


type alias Model =
    { current : PageLoaded
    }


updateCurrent : PageLoaded -> Model -> Model
updateCurrent current model =
    { model
        | current = current
    }


init : Bag -> Router.Route -> ( Model, Cmd Msg )
init bag route =
    let
        model =
            { current = None
            }
    in
    enterRoute bag model route


enterRoute : Bag -> Model -> Router.Route -> ( Model, Cmd Msg )
enterRoute bag model route =
    case route of
        Router.Index ->
            let
                ( subModel, subCmd ) =
                    Pages.Index.init

                current =
                    InternalPage.Index subModel ()
            in
            ( updateCurrent current model
            , Cmd.batch [ Cmd.map IndexMsg subCmd ]
            )

        Router.About ->
            let
                ( subModel, subCmd ) =
                    Pages.About.init

                current =
                    InternalPage.About subModel ()
            in
            ( updateCurrent current model
            , Cmd.batch [ Cmd.map AboutMsg subCmd ]
            )

        Router.Counter ->
            let
                ( subModel, subCmd ) =
                    Pages.Counter.init

                current =
                    InternalPage.Counter subModel ()
            in
            ( updateCurrent current model
            , Cmd.batch [ Cmd.map CounterMsg subCmd ]
            )

        Router.CounterAsync ->
            let
                ( subModel, subCmd ) =
                    Pages.CounterAsync.init

                current =
                    InternalPage.CounterAsync subModel ()
            in
            ( updateCurrent current model
            , Cmd.batch [ Cmd.map CounterAsyncMsg subCmd ]
            )


type Msg
    = CounterMsg Pages.Counter.Msg
    | CounterAsyncMsg Pages.CounterAsync.Msg


type ExternalMsg
    = NoExternalMsg


update : Bag -> Msg -> Model -> ( Model, Cmd Msg, Cmd ExternalMsg )
update bag msg model =
    case ( msg, model.current ) of
        ( CounterMsg subMsg, InternalPage.Counter subModel_prev _ ) ->
            let
                ( subModel, subCmd ) =
                    Pages.Counter.update subMsg subModel_prev

                current =
                    InternalPage.Counter subModel ()
            in
            ( updateCurrent current model
            , Cmd.map CounterMsg subCmd
            , Cmd.none
            )

        ( CounterAsyncMsg subMsg, InternalPage.CounterAsync subModel_prev _ ) ->
            let
                ( subModel, subCmd ) =
                    Pages.CounterAsync.update subMsg subModel_prev

                current =
                    InternalPage.CounterAsync subModel ()
            in
            ( updateCurrent current model
            , Cmd.map CounterAsyncMsg subCmd
            , Cmd.none
            )


view : Bag -> Model -> Document Msg
view bag model =
    let
        result =
            case model.current of
                InternalPage.Index _ _ ->
                    let
                        subBag =
                            { urls = Router.urls bag.router.navigationKey
                            }

                        subView =
                            Pages.Index.view subBag
                            |> Document.map IndexMsg
                    in
                    subView

                InternalPage.About _ _ ->
                    let
                        subBag =
                            { urls = Router.urls bag.router.navigationKey
                            }

                        subView =
                            Pages.About.view subBag
                            |> Document.map AboutMsg
                    in
                    subView

                InternalPage.Counter subModel _ ->
                    let
                        subBag =
                            { urls = Router.urls bag.router.navigationKey
                            }

                        subView =
                            Pages.Counter.view subBag subModel
                            |> Document.map CounterMsg
                    in
                    subView

                InternalPage.CounterAsync subModel _ ->
                    let
                        subBag =
                            { urls = Router.urls bag.router.navigationKey
                            }

                        subView =
                            Pages.CounterAsync.view subBag subModel
                            |> Document.map CounterAsyncMsg
                    in
                    subView

                InternalPage.NotFound _ _ ->
                    let
                        subBag =
                            { urls = Router.urls bag.router.navigationKey
                            }

                        subView =
                            Pages.NotFound.view
                            |> Document.map NotFoundMsg
                    in
                    subView


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.current of
        InternalPage.CounterAsync subModel _ ->
            Sub.map CounterAsyncMsg (Pages.CounterAsync.subscriptions subModel)

        _ ->
            Sub.none
"""
