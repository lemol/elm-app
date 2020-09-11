module ElmApp.Spa.MainModule exposing (..)

import Elm.CodeGen exposing (..)
import ElmApp.Error exposing (Error)
import ElmApp.Module exposing (DocumentInfo(..), Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmApp.Spa exposing (init)
import ElmApp.Spa.PagesModule exposing (pushUrlDecl)


write : { documentInfo : DocumentInfo } -> Result Error File
write opts =
    let
        moduleName =
            [ "App", "Main" ]

        file_ =
            file
                (normalModule moduleName (funExpose "main"))
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
            ]
    in
    Ok file_


mainDecl : Declaration
mainDecl =
    funDecl Nothing
        (Just
            (typed "Program"
                [ typed "Flags" []
                , typed "Model" []
                , typed "Msg" []
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
            (funAnn (typed "Flags" [])
                (funAnn (fqTyped [ "Url" ] "Url" [])
                    (funAnn (fqTyped [ "Navigation" ] "Key" [])
                        (tupleAnn
                            [ typed "Model" []
                            , typed "Cmd" [ typed "Msg" [] ]
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
                (access (val "model") "router")
            , letFunction "route"
                []
                (apply [ fun "parseUrl", val "url" ])
            , letFunction "newApp"
                []
                (update "router" [ ( "route", val "route" ) ])
            , letFunction "bag"
                []
                (record
                    [ ( "global", access (val "model") "global" )
                    , ( "router", val "newApp" )
                    ]
                )
            , letDestructuring
                (tuplePattern [ varPattern "newPage", varPattern "newPageCmd" ])
                (apply [ fqFun [ "Page" ] "enterRoute", val "bag", access (val "model") "page", val "route" ])
            ]
            (tuple
                [ val "model"
                , apply
                    [ fqFun [ "Cmd" ] "batch"
                    , list
                        [ apply
                            [ fqFun [ "Cmd" ] "map"
                            , construct "GlobalMsg" []
                            , val "globalCmd"
                            ]
                        , apply
                            [ fqFun [ "Cmd" ] "map"
                            , construct "PageMsg" []
                            , val "newPageCmd"
                            ]
                        ]
                    ]
                ]
            )
        )



-- UTILS


documentImports : DocumentInfo -> List Import
documentImports info =
    case info of
        DocumentModule name ->
            [ importStmt name (Just [ "Document" ]) Nothing ]

        DocumentModuleCustom name _ _ ->
            [ importStmt name (Just [ "Document" ]) Nothing ]
