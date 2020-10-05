module Spa.Fixtures.BasicPages.Pages_Internal_Main_Elm exposing (..)


contentSimple : String
contentSimple =
    """module App.Pages.Internal.Main exposing (ExternalMsg(..), Model, Msg(..), enterRoute, init, subscriptions, update, view)

import App.Pages
import App.Pages.Internal.NotFound
import App.Pages.Internal.Page as InternalPage
import App.Pages.Internal.Router as Router
import Browser.Navigation as Navigation
import Html
import Pages.About
import Pages.Counter
import Pages.CounterAsync
import Pages.Index


type alias Bag =
    { router : Router.Model }


type alias Model =
    { current : InternalPage.PageLoaded }


updateCurrent : InternalPage.PageLoaded -> Model -> Model
updateCurrent current model =
    { model | current = current }


init : Bag -> ( Model, Cmd Msg )
init bag =
    let
        model =
            { current = InternalPage.None }
    in
    enterRoute bag model


type Msg
    = NoMsg
    | CounterMsg Pages.Counter.Msg
    | CounterAsyncMsg Pages.CounterAsync.Msg


type ExternalMsg
    = NoExternalMsg


enterRoute : Bag -> Model -> ( Model, Cmd Msg, Cmd ExternalMsg )
enterRoute bag model =
    case bag.router.route of
        Router.Index ->
            let
                current =
                    InternalPage.Index () ()
            in
            ( updateCurrent current model, Cmd.batch [], Cmd.none )

        Router.About ->
            let
                current =
                    InternalPage.About () ()
            in
            ( updateCurrent current model, Cmd.batch [], Cmd.none )

        Router.Counter ->
            let
                subModel =
                    Pages.Counter.init

                current =
                    InternalPage.Counter subModel ()
            in
            ( updateCurrent current model, Cmd.batch [], Cmd.none )

        Router.CounterAsync ->
            let
                ( subModel, subCmd ) =
                    Pages.CounterAsync.init

                current =
                    InternalPage.CounterAsync subModel ()
            in
            ( updateCurrent current model, Cmd.batch [ Cmd.map CounterAsyncMsg subCmd ], Cmd.none )

        Router.NotFound ->
            let
                current =
                    InternalPage.NotFound () ()
            in
            ( updateCurrent current model, Cmd.batch [], Cmd.none )


update : Bag -> Msg -> Model -> ( Model, Cmd Msg, Cmd ExternalMsg )
update bag msg model =
    case ( msg, model.current ) of
        ( CounterMsg subMsg, InternalPage.Counter subModel_prev _ ) ->
            let
                subModel =
                    Pages.Counter.update subMsg subModel_prev

                current =
                    InternalPage.Counter subModel ()
            in
            ( updateCurrent current model, Cmd.none, Cmd.none )

        ( CounterAsyncMsg subMsg, InternalPage.CounterAsync subModel_prev _ ) ->
            let
                ( subModel, subCmd ) =
                    Pages.CounterAsync.update subMsg subModel_prev

                current =
                    InternalPage.CounterAsync subModel ()
            in
            ( updateCurrent current model, Cmd.map CounterAsyncMsg subCmd, Cmd.none )


view : Bag -> Model -> Html.Document Msg
view bag model =
    let
        result =
            case model.current of
                InternalPage.Index _ _ ->
                    let
                        subBag =
                            { urls = Router.urls bag.router.navigationKey }

                        subView =
                            Pages.Index.view subBag
                    in
                    subView |> htmlDocumentMap IndexMsg

                InternalPage.About _ _ ->
                    let
                        subBag =
                            { urls = Router.urls bag.router.navigationKey }

                        subView =
                            Pages.About.view subBag
                    in
                    subView |> htmlDocumentMap AboutMsg

                InternalPage.Counter subModel _ ->
                    let
                        subBag =
                            { urls = Router.urls bag.router.navigationKey }

                        subView =
                            Pages.Counter.view subBag subModel
                    in
                    subView |> htmlDocumentMap CounterMsg

                InternalPage.CounterAsync subModel _ ->
                    let
                        subBag =
                            { urls = Router.urls bag.router.navigationKey }

                        subView =
                            Pages.CounterAsync.view subBag subModel
                    in
                    subView |> htmlDocumentMap CounterAsyncMsg

                InternalPage.NotFound _ _ ->
                    let
                        subView =
                            App.Pages.Internal.NotFound.view
                    in
                    subView |> htmlDocumentMap NotFoundMsg
    in
    result


subscriptions : Bag -> Model -> Sub Msg
subscriptions bag model =
    case model.current of
        InternalPage.CounterAsync subModel _ ->
            let
                subSub =
                    Pages.CounterAsync.subscriptions subModel
            in
            Sub.map CounterAsyncMsg subSub

        _ ->
            Sub.none


htmlDocumentMap : (msg1 -> msg2) -> Html.Document msg1 -> Html.Document msg2
htmlDocumentMap f doc =
    { title = doc.title, body = doc.body |> List.map Html.map }
"""
