module ElmApp.Spa.RouterModule exposing (..)

import Elm.CodeGen exposing (..)
import ElmApp.Error exposing (Error)
import ElmApp.Module exposing (Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmApp.Spa.Config exposing (Config)
import ElmApp.Spa.Route as Route
import ElmApp.Spa.RouteCodeGen as RouteCodeGen


type alias Options =
    { config : Config
    , pages : List Module
    }


write : Options -> Result Error File
write opts =
    let
        moduleName =
            [ "App", "Pages", "Internal", "Router" ]

        file_ =
            file
                (normalModule moduleName exposing_)
                [ importStmt
                    [ "Browser", "Navigation" ]
                    (Just [ "Navigation" ])
                    Nothing
                , importStmt
                    [ "Url" ]
                    Nothing
                    (Just (exposeExplicit [ closedTypeExpose "Url" ]))
                , importStmt
                    [ "Url", "Parser" ]
                    Nothing
                    (Just
                        (exposeExplicit
                            [ funExpose "(</>)"
                            , closedTypeExpose "Parser"
                            , funExpose "map"
                            , funExpose "oneOf"
                            , funExpose "parse"
                            , funExpose "s"
                            , funExpose "string"
                            , funExpose "top"
                            ]
                        )
                    )
                ]
                declarations
                Nothing

        exposing_ =
            [ typeOrAliasExpose "AppUrl"
            , typeOrAliasExpose "AppUrls"
            , typeOrAliasExpose "Model"
            , openTypeExpose "Route"
            , funExpose "init"
            , funExpose "parseUrl"
            , funExpose "toPath"
            , funExpose "urlKey"
            , funExpose "urlRoute"
            , funExpose "urls"
            ]

        declarations =
            [ modelDecl
            , initDecl
            , routeTypeDecl opts
            , parseUrlDecl
            , toPathDecl opts
            , matchRouteDecl opts
            , appUrlTypeDecl
            , appUrlsTypeDecl opts
            , appUrlsDecl opts
            , urlKeyDecl
            , urlRouteDecl
            ]
    in
    Ok file_


modelDecl : Declaration
modelDecl =
    aliasDecl Nothing
        "Model"
        []
        (recordAnn
            [ ( "navigationKey", fqTyped [ "Navigation" ] "Key" [] )
            , ( "route", typed "Route" [] )
            ]
        )


initDecl : Declaration
initDecl =
    funDecl Nothing
        (Just
            (funAnn
                (fqTyped [ "Navigation" ] "Key" [])
                (funAnn
                    (typed "Route" [])
                    (typed "Model" [])
                )
            )
        )
        "init"
        [ varPattern "key", varPattern "route" ]
        (record
            [ ( "navigationKey", val "key" )
            , ( "route", val "route" )
            ]
        )


routeTypeDecl : Options -> Declaration
routeTypeDecl opts =
    ((opts.config.routes
        |> List.map RouteCodeGen.routeContructTA
     )
        ++ [ ( "NotFound", [] ) ]
    )
        |> customTypeDecl Nothing "Route" []


parseUrlDecl : Declaration
parseUrlDecl =
    funDecl Nothing
        (Just
            (funAnn
                (typed "Url" [])
                (typed "Route" [])
            )
        )
        "parseUrl"
        [ varPattern "url" ]
        (caseExpr
            (apply
                [ fun "parse", val "matchRoute", val "url" ]
            )
            [ ( namedPattern "Just" [ varPattern "route" ], val "route" )
            , ( namedPattern "Nothing" [], construct "NotFound" [] )
            ]
        )


toPathDecl : Options -> Declaration
toPathDecl opts =
    let
        caseBranch route =
            ( RouteCodeGen.routeUnconstructPattern route
            , RouteCodeGen.routePath route
            )
    in
    funDecl Nothing
        (Just
            (funAnn
                (typed "Route" [])
                stringAnn
            )
        )
        "toPath"
        [ varPattern "route" ]
        (((opts.config.routes
            |> List.map caseBranch
          )
            ++ [ ( namedPattern "NotFound" []
                 , applyBinOp (string "/") append (string "404")
                 )
               ]
         )
            |> caseExpr (val "route")
        )


matchRouteDecl : Options -> Declaration
matchRouteDecl opts =
    funDecl Nothing
        (Just
            (typed "Parser"
                [ funAnn
                    (typed "Route" [])
                    (typeVar "a")
                , typeVar "a"
                ]
            )
        )
        "matchRoute"
        []
        (apply
            [ fun "oneOf"
            , ((opts.config.routes
                    |> List.map RouteCodeGen.routeMatch
               )
                ++ [ apply
                        [ fun "map"
                        , construct "NotFound" []
                        , apply
                            [ fun "s", string "404" ]
                            |> parens
                        ]
                   ]
              )
                |> list
            ]
        )


appUrlTypeDecl : Declaration
appUrlTypeDecl =
    customTypeDecl Nothing
        "AppUrl"
        []
        [ ( "AppUrl"
          , [ fqTyped [ "Navigation" ] "Key" []
            , typed "Route" []
            ]
          )
        ]


appUrlsTypeDecl : Options -> Declaration
appUrlsTypeDecl opts =
    aliasDecl Nothing
        "AppUrls"
        []
        (opts.config.routes
            |> List.map RouteCodeGen.routeUrlRecordTA
            |> recordAnn
        )


appUrlsDecl : Options -> Declaration
appUrlsDecl opts =
    funDecl Nothing
        (Just
            (funAnn
                (fqTyped [ "Navigation" ] "Key" [])
                (typed "AppUrls" [])
            )
        )
        "urls"
        [ varPattern "navigationKey" ]
        (opts.config.routes
            |> List.map RouteCodeGen.routeUrlRecord
            |> record
        )


urlKeyDecl : Declaration
urlKeyDecl =
    funDecl Nothing
        (Just
            (funAnn
                (typed "AppUrl" [])
                (fqTyped [ "Navigation" ] "Key" [])
            )
        )
        "urlKey"
        [ namedPattern "AppUrl" [ varPattern "key", allPattern ]
            |> parensPattern
        ]
        (val "key")


urlRouteDecl : Declaration
urlRouteDecl =
    funDecl Nothing
        (Just
            (funAnn
                (typed "AppUrl" [])
                (typed "Route" [])
            )
        )
        "urlRoute"
        [ namedPattern "AppUrl" [ allPattern, varPattern "route" ]
            |> parensPattern
        ]
        (val "route")



-- UTILS


routeName : Module -> String
routeName module_ =
    "Router." ++ pageName module_


pageName : Module -> String
pageName module_ =
    case module_.name of
        "Pages" :: xs ->
            String.join "" xs

        xs ->
            String.join "" xs
