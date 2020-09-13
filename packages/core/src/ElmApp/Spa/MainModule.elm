module ElmApp.Spa.MainModule exposing (..)

import Elm.CodeGen exposing (..)
import ElmApp.Error exposing (Error)
import ElmApp.Module exposing (DocumentInfo(..), Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmApp.Spa.PagesModule exposing (pushUrlDecl)
import ElmApp.Spa.Utils as Utils exposing (..)


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
            , ( "global", fqTyped [ "Global" ] "Model" [] )
            , ( "Page", fqTyped [ "Page" ] "Model" [] )
            ]
        )


msgDecl : Declaration
msgDecl =
    customTypeDecl Nothing
        "Model"
        []
        [ ( "LinkClicked", [ fqTyped [ "Browser" ] "UrlRequest" [] ] )
        , ( "UrlChanged", [ fqTyped [ "Url" ] "Url" [] ] )
        , ( "GlobalMsg", [ fqTyped [ "Page" ] "Msg" [] ] )
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
            [ letFunction "router"
                []
                (modelAccess "router")
            , letFunction "route"
                []
                (apply [ fun "parseUrl", val "url" ])
            , letFunction "newRouter"
                []
                (update "router" [ ( "route", val "route" ) ])
            , letFunction "bag"
                []
                (record
                    [ ( "global", modelAccess "global" )
                    , ( "router", val "newRouter" )
                    ]
                )
            , letDestructuring
                (tuplePattern [ varPattern "newPage", varPattern "newPageCmd" ])
                (apply [ fqFun [ "Page" ] "enterRoute", val "bag", modelAccess "page", val "route" ])
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
            ]
        )



-- UTILS


documentImports : DocumentInfo -> List Import
documentImports info =
    case info of
        DocumentModule name ->
            [ importStmt name (Just [ "Document" ]) Nothing ]

        DocumentModuleCustom name _ _ ->
            [ importStmt name (Just [ "Document" ]) Nothing ]
