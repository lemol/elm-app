module ElmApp.Spa.Writers.Pages_Internal_Main_Elm exposing (..)

import Elm.CodeGen exposing (..)
import ElmApp.Error exposing (Error)
import ElmApp.Module as Module exposing (Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmApp.Spa.Config as Config exposing (Config)
import ElmApp.Spa.Route as Route exposing (Route)
import ElmApp.Spa.RouteCodeGen exposing (fqRouteUnconstructPattern)
import ElmApp.Spa.Utils
    exposing
        ( bagVal
        , cmdBatch
        , cmdMap
        , cmdNone
        , modelAccess
        , modelCmdMsgExternalTA
        , modelCmdMsgTA
        , modelTA
        , modelVal
        , msgTA
        , msgVal
        , subMsgTA
        , subNone
        )
import ElmApp.Spa.Writers.Internal.Pages_Internal_Main_Elm as Internal


type alias Options =
    { config : Config
    , routes : List Route
    }


write : Options -> Result Error File
write opts =
    let
        moduleName =
            [ "App", "Pages", "Internal", "Main" ]

        file_ =
            file
                (normalModule moduleName exposing_)
                (importList opts)
                declarations
                Nothing

        exposing_ =
            [ openTypeExpose "ExternalMsg"
            , typeOrAliasExpose "Model"
            , openTypeExpose "Msg"
            , funExpose "enterRoute"
            , funExpose "init"
            , funExpose "subscriptions"
            , funExpose "update"
            , funExpose "view"
            ]

        declarations =
            [ bagTypeDecl
            , modelTypeDecl
            , updateCurrentFunDecl
            , initFunDecl
            , msgTypeDecl opts
            , externalMsgTypeDecl opts
            , enterRouteFunDecl opts
            , updateFunDecl opts
            , viewFunDecl opts
            , subscriptionsFunDecl opts
            , htmlDocumentMapFunDecl opts
            ]
    in
    Ok file_


importList : Options -> List Import
importList opts =
    let
        pages =
            opts.routes
                |> List.map (Route.module_ >> .name)
                |> List.map (\name -> importStmt name Nothing Nothing)
    in
    pages
        ++ [ importStmt [ "App", "Pages" ] Nothing Nothing
           , importStmt [ "App", "Pages", "Internal", "Page" ] (Just [ "InternalPage" ]) Nothing
           , importStmt [ "App", "Pages", "Internal", "Router" ] (Just [ "Router" ]) Nothing
           , importStmt [ "Browser", "Navigation" ] (Just [ "Navigation" ]) Nothing
           , importStmt (Config.documentModuleName opts.config.document) Nothing Nothing
           ]


bagTypeDecl : Declaration
bagTypeDecl =
    aliasDecl Nothing
        "Bag"
        []
        ([ ( "router", fqTyped [ "Router" ] "Model" [] ) ]
            |> recordAnn
        )


modelTypeDecl : Declaration
modelTypeDecl =
    aliasDecl Nothing
        "Model"
        []
        ([ ( "current", fqTyped [ "InternalPage" ] "PageLoaded" [] ) ]
            |> recordAnn
        )


updateCurrentFunDecl : Declaration
updateCurrentFunDecl =
    funDecl Nothing
        (Just <|
            funAnn (fqTyped [ "InternalPage" ] "PageLoaded" [])
                (funAnn (typed "Model" []) (typed "Model" []))
        )
        "updateCurrent"
        [ varPattern "current", varPattern "model" ]
        (update "model" [ ( "current", val "current" ) ])


initFunDecl : Declaration
initFunDecl =
    funDecl Nothing
        (Just <|
            funAnn (typed "Bag" [])
                modelCmdMsgTA
        )
        "init"
        [ varPattern "bag" ]
        (letExpr
            [ letFunction "model"
                []
                (record [ ( "current", construct "InternalPage.None" [] ) ])
            ]
            (apply
                [ fun "enterRoute"
                , bagVal
                , modelVal
                ]
            )
        )


msgTypeDecl : Options -> Declaration
msgTypeDecl opts =
    let
        pageMsgContructTA route =
            Route.module_ route
                |> Module.msgAnn
                |> Maybe.map
                    (\msgAnn ->
                        ( pageMsg route, [ msgAnn ] )
                    )
    in
    (( "NoMsg", [] )
        :: (opts.routes
                |> List.filterMap pageMsgContructTA
           )
    )
        |> customTypeDecl Nothing "Msg" []


externalMsgTypeDecl : Options -> Declaration
externalMsgTypeDecl _ =
    [ ( "NoExternalMsg", [] ) ]
        |> customTypeDecl Nothing "ExternalMsg" []


enterRouteFunDecl : Options -> Declaration
enterRouteFunDecl opts =
    let
        routeCase route =
            ( fqRouteUnconstructPattern [ "Router" ] route
            , letExpr
                ([ Module.initApplyDestruct
                    { model = "subModel"
                    , cmd = "subCmd"
                    }
                    (Route.module_ route)
                 , Module.paramsLetDeclaration (Route.module_ route)
                 , letDestructuring (varPattern "current")
                    (pageLoadedConstruct
                        { model =
                            if Module.hasModel (Route.module_ route) then
                                val "subModel"

                            else
                                unit
                        , params =
                            if Module.hasParams (Route.module_ route) then
                                val "params"

                            else
                                unit
                        }
                        route
                    )
                    |> Just
                 ]
                    |> List.filterMap identity
                )
                (tuple
                    [ apply
                        [ fun "updateCurrent"
                        , val "current"
                        , modelVal
                        ]
                    , [ if Module.initHasCmd (Route.module_ route) then
                            Just (cmdMap (pageMsg route) (val "subCmd"))

                        else
                            Nothing
                      ]
                        |> List.filterMap identity
                        |> cmdBatch
                    , cmdNone
                    ]
                )
            )
    in
    funDecl Nothing
        (Just <|
            funAnn (typed "Bag" [])
                (funAnn modelTA modelCmdMsgExternalTA)
        )
        "enterRoute"
        [ varPattern "bag", varPattern "model" ]
        (opts.routes
            |> List.map routeCase
            |> caseExpr (access (access (val "bag") "router") "route")
        )


updateFunDecl : Options -> Declaration
updateFunDecl opts =
    let
        msgCase route =
            Internal.updateFunCaseBranch route
    in
    funDecl Nothing
        (Just <|
            funAnn (typed "Bag" [])
                (funAnn msgTA
                    (funAnn modelTA modelCmdMsgExternalTA)
                )
        )
        "update"
        [ varPattern "bag", varPattern "msg", varPattern "model" ]
        (opts.routes
            |> List.filterMap msgCase
            |> caseExpr (tuple [ msgVal, modelAccess "current" ])
        )


viewFunDecl : Options -> Declaration
viewFunDecl opts =
    let
        viewCase route =
            Internal.viewFunCaseBranch opts.config route

        resultExpr =
            opts.routes
                |> List.filterMap viewCase
                |> caseExpr (modelAccess "current")

        documentTA =
            fqTyped
                (Config.documentModuleName opts.config.document)
                (Config.documentTypeName opts.config.document)
                [ msgTA ]
    in
    funDecl Nothing
        (Just <|
            funAnn (typed "Bag" [])
                (funAnn modelTA documentTA)
        )
        "view"
        [ varPattern "bag", varPattern "model" ]
        (letExpr
            [ letVal "result" resultExpr ]
            (val "result")
        )


subscriptionsFunDecl : Options -> Declaration
subscriptionsFunDecl opts =
    let
        subCase route =
            Internal.subFunCaseBranch opts.config route
    in
    funDecl Nothing
        (Just <|
            funAnn (typed "Bag" [])
                (funAnn modelTA subMsgTA)
        )
        "subscriptions"
        [ varPattern "bag", varPattern "model" ]
        (opts.routes
            |> List.filterMap subCase
            |> (\cases ->
                    cases
                        ++ [ ( allPattern
                             , subNone
                             )
                           ]
               )
            |> caseExpr (modelAccess "current")
        )


htmlDocumentMapFunDecl : Options -> Declaration
htmlDocumentMapFunDecl opts =
    let
        docAnn msg =
            fqTyped [ "Html" ] "Document" [ typeVar msg ]
    in
    funDecl Nothing
        (Just <|
            funAnn
                (funAnn (typeVar "msg1") (typeVar "msg2"))
                (funAnn (docAnn "msg1") (docAnn "msg2"))
        )
        "htmlDocumentMap"
        [ varPattern "f", varPattern "doc" ]
        (record
            [ ( "title", access (val "doc") "title" )
            , ( "body"
              , pipe (access (val "doc") "body")
                    [ apply
                        [ fqFun [ "List" ] "map"
                        , fqFun [ "Html" ] "map"
                        ]
                    ]
              )
            ]
        )



-- htmlDocumentMap : (msg1 -> msg2) -> Document msg1 -> Document msg2
-- htmlDocumentMap f doc =
--     { title = doc.title, body = doc.body |> List.map Html.map }
-- UTILS


pageMsg : Route -> String
pageMsg route =
    Route.name route ++ "Msg"


pageLoadedConstruct :
    { model : Expression
    , params : Expression
    }
    -> Route
    -> Expression
pageLoadedConstruct opts route =
    fqConstruct [ "InternalPage" ]
        (Route.name route)
        [ opts.model, opts.params ]
