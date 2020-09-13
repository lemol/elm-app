module ElmApp.Spa.MainModule exposing (..)

import Elm.CodeGen exposing (..)
import Elm.Syntax.Expression exposing (Expression)
import ElmApp.Error exposing (Error)
import ElmApp.Module exposing (DocumentInfo(..), Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmApp.Spa.PagesModule exposing (pushUrlDecl)
import ElmApp.Spa.Utils as Utils exposing (..)
import Html.Attributes exposing (name)


write : { documentInfo : DocumentInfo } -> Result Error File
write opts =
    let
        moduleName =
            [ "App", "Main" ]

        file_ =
            file
                (normalModule moduleName [ funExpose "main" ])
                ([ importStmt
                    [ "App", "Pages", "Internal", "Router" ]
                    (Just [ "Router" ])
                    Nothing
                 , importStmt
                    [ "App", "Pages", "Main" ]
                    (Just [ "Page" ])
                    Nothing
                 , importStmt
                    [ "Browser" ]
                    Nothing
                    Nothing
                 , importStmt
                    [ "Browser", "Navigation" ]
                    (Just [ "Navigation" ])
                    Nothing
                 , importStmt
                    [ "Url" ]
                    Nothing
                    Nothing
                 ]
                    ++ documentImports opts.documentInfo
                 -- ++ globalImport
                )
                declarations
                Nothing

        declarations =
            [ mainDecl
            , flagsDecl
            , modelDecl
            , msgDecl
            , initDecl
            , updateDecl
            , subscriptionsDecl
            , viewDecl opts.documentInfo
            ]
    in
    Ok file_


mainDecl : Declaration
mainDecl =
    funDecl Nothing
        (Just
            (typed "Program"
                [ flagsTA
                , modelTA
                , msgTA
                ]
            )
        )
        "main"
        []
        (apply
            [ fqFun [ "Browser" ] "application"
            , record
                [ ( "init", fun "init" )
                , ( "update", fun "update" )
                , ( "view", fun "view" )
                , ( "subscriptions", fun "subscriptions" )
                , ( "onUrlRequest", construct "LinkClicked" [] )
                , ( "onUrlChange", construct "UrlChanged" [] )
                ]
            ]
        )


flagsDecl : Declaration
flagsDecl =
    aliasDecl Nothing "Flags" [] unitAnn


modelDecl : Declaration
modelDecl =
    aliasDecl Nothing
        "Model"
        []
        (recordAnn
            [ ( "router", fqTyped [ "Router" ] "Model" [] )

            --, ( "global", fqTyped [ "Global" ] "Model" [] )
            , ( "page", fqTyped [ "Page" ] "Model" [] )
            ]
        )


msgDecl : Declaration
msgDecl =
    customTypeDecl Nothing
        "Msg"
        []
        [ ( "LinkClicked", [ fqTyped [ "Browser" ] "UrlRequest" [] ] )
        , ( "UrlChanged", [ fqTyped [ "Url" ] "Url" [] ] )

        --, ( "GlobalMsg", [ fqTyped [ "Page" ] "Msg" [] ] )
        , ( "PageMsg", [ fqTyped [ "Page" ] "Msg" [] ] )
        ]


initDecl : Declaration
initDecl =
    funDecl Nothing
        (Just
            (funAnn flagsTA
                (funAnn (fqTyped [ "Url" ] "Url" [])
                    (funAnn (fqTyped [ "Navigation" ] "Key" [])
                        (tupleAnn
                            [ modelTA
                            , cmdMsgTA
                            ]
                        )
                    )
                )
            )
        )
        "init"
        [ allPattern
        , varPattern "url"
        , varPattern "key"
        ]
        (letExpr
            [ letFunction "route"
                []
                (apply [ fun "parseUrl", val "url" ])
            , letFunction "router"
                []
                (apply
                    [ fqFun [ "Router" ] "init"
                    , val "key"
                    , val "route"
                    ]
                )
            , letFunction "bag"
                []
                (record
                    [ ( "router", val "router" )
                    ]
                )
            , letDestructuring
                (tuplePattern [ varPattern "newPage", varPattern "newPageCmd" ])
                (apply [ fqFun [ "Page" ] "init", val "bag", val "route" ])
            , letFunction "model"
                []
                (record
                    [ ( "router", val "router" )
                    , ( "page", val "newPage" )
                    ]
                )
            ]
            (tuple
                [ modelVal
                , cmdBatch
                    [ cmdMap "PageMsg" (val "newPageCmd")
                    ]
                ]
            )
        )


updateDecl : Declaration
updateDecl =
    Utils.updateDecl
        (caseExpr msgVal
            [ ( namedPattern "LinkClicked" [ varPattern "urlRequest" ]
              , caseExpr (val "urlRequest")
                    [ ( namedPattern "Browser.Internal" [ varPattern "url" ]
                      , tuple
                            [ modelVal
                            , apply
                                [ fqFun [ "Navigation" ] "pushUrl"
                                , access (access modelVal "router") "navigationKey"
                                , apply
                                    [ fqFun [ "Url" ] "toString", val "url" ]
                                    |> parens
                                ]
                            ]
                      )
                    , ( namedPattern "Browser.External" [ varPattern "href" ]
                      , tuple
                            [ modelVal
                            , apply
                                [ fqFun [ "Navigation" ] "load"
                                , val "href"
                                ]
                            ]
                      )
                    ]
              )
            , ( namedPattern "UrlChanged" [ varPattern "url" ]
              , letExpr
                    [ letFunction "router" [] (modelAccess "router")
                    , letFunction "route"
                        []
                        (apply [ fun "parseUrl", val "url" ])
                    , letFunction "newRouter"
                        []
                        (update "router" [ ( "route", val "route" ) ])
                    , letFunction "bag"
                        []
                        (record
                            [ ( "router", val "newRouter" )
                            ]
                        )
                    , letDestructuring
                        (tuplePattern [ varPattern "newPage", varPattern "newPageCmd" ])
                        (apply [ fqFun [ "Page" ] "enterRoute", val "bag", modelAccess "page", val "route" ])
                    ]
                    (tuple
                        [ update "model"
                            [ ( "page", val "newPage" )
                            , ( "router", val "newRouter" )
                            ]
                        , cmdBatch
                            [ cmdMap "PageMsg" (val "newPageCmd")
                            ]
                        ]
                    )
              )
            , ( namedPattern "PageMsg" [ varPattern "subMsg" ]
              , letExpr
                    [ letFunction "bag"
                        []
                        (record
                            [ ( "router", modelAccess "router" )
                            ]
                        )
                    , letDestructuring
                        (tuplePattern [ varPattern "newPage", varPattern "newPageCmd" ])
                        (apply [ fqFun [ "Page" ] "update", val "bag", val "subMsg", modelAccess "page" ])
                    ]
                    (tuple
                        [ update "model"
                            [ ( "page", val "newPage" )
                            ]
                        , cmdBatch
                            [ cmdMap "PageMsg" (val "newPageCmd")
                            ]
                        ]
                    )
              )
            ]
        )


subscriptionsDecl : Declaration
subscriptionsDecl =
    funDecl Nothing
        (Just
            (funAnn modelTA subMsgTA)
        )
        "subscriptions"
        [ varPattern "model"
        ]
        (subBatch
            [ subMap "PageMsg"
                (apply
                    [ fqFun [ "Page" ] "subscriptions"
                    , modelAccess "page"
                    ]
                    |> parens
                )
            ]
        )


viewDecl : DocumentInfo -> Declaration
viewDecl docInfo =
    funDecl Nothing
        (Just
            (funAnn modelTA
                (fqTyped [ "Browser" ] "Document" [ typed "Msg" [] ])
            )
        )
        "view"
        [ varPattern "model"
        ]
        (letExpr
            [ letFunction "bag"
                []
                (record
                    [ ( "router", modelAccess "router" ) ]
                )
            ]
            (pipe
                (apply
                    [ fqFun [ "Page" ] "view"
                    , val "bag"
                    , modelAccess "page"
                    ]
                )
                [ apply
                    ([ documentMap docInfo
                     , construct "PageMsg" []
                     ]
                        ++ documentToBrowser docInfo
                    )
                ]
            )
        )



-- UTILS


documentImports : DocumentInfo -> List Import
documentImports info =
    case info of
        DocumentModule name ->
            [ importStmt name Nothing Nothing ]

        DocumentModuleCustom name _ _ ->
            [ importStmt name Nothing Nothing ]


documentMap : DocumentInfo -> Expression
documentMap info =
    case info of
        DocumentModule name ->
            fqFun name "map"

        DocumentModuleCustom name map _ ->
            fqFun name map


documentToBrowser : DocumentInfo -> List Expression
documentToBrowser info =
    case info of
        DocumentModule [ "Html" ] ->
            []

        DocumentModule name ->
            [ fqFun name "toBrowserDocument" ]

        DocumentModuleCustom name _ toBrowser ->
            [ fqFun name toBrowser ]
