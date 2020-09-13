module ElmApp.Spa.PagesModule exposing (..)

import Elm.CodeGen exposing (..)
import ElmApp.Error exposing (Error)
import ElmApp.Module exposing (Module)
import ElmApp.ModuleType exposing (ModuleType(..))


write : List Module -> Result Error File
write pages =
    let
        moduleName =
            [ "App", "Pages" ]

        file_ =
            file
                (normalModule moduleName exposing_)
                [ importStmt
                    [ "App", "Pages", "Internal", "Router" ]
                    (Just [ "Router" ])
                    Nothing
                , importStmt
                    [ "Browser", "Navigation" ]
                    (Just [ "Navigation" ])
                    Nothing
                , importStmt
                    [ "Task" ]
                    Nothing
                    Nothing
                ]
                declarations
                Nothing

        exposing_ =
            [ typeOrAliasExpose "AppUrl"
            , typeOrAliasExpose "AppUrls"
            , openTypeExpose "Page"
            , funExpose "async"
            , funExpose "fromRoute"
            , funExpose "pushUrl"
            , funExpose "urlPage"
            , funExpose "urlString"
            ]

        declarations =
            [ asyncDecl
            , pushUrlDecl
            , pageTypeDecl pages
            , fromRouteDecl pages
            , appUrlDecl
            , appUrlsDecl
            , urlPageDecl
            , urlStringDecl
            ]
    in
    Ok file_


asyncDecl : Declaration
asyncDecl =
    funDecl Nothing
        (Just
            (funAnn
                (typeVar "msg")
                (typed "Cmd" [ typeVar "msg" ])
            )
        )
        "async"
        []
        (chain
            (fqFun [ "Task" ] "succeed")
            [ apply
                [ fqFun [ "Task" ] "perform"
                , fun "identity"
                ]
            ]
        )


pushUrlDecl : Declaration
pushUrlDecl =
    funDecl Nothing
        (Just
            (funAnn
                (typed "AppUrl" [])
                (typed "Cmd" [ typeVar "msg" ])
            )
        )
        "pushUrl"
        [ varPattern "url" ]
        (apply
            [ fqFun [ "Navigation" ] "pushUrl"
            , apply
                [ fqFun [ "Router" ] "urlKey"
                , val "url"
                ]
                |> parens
            , apply
                [ fqFun [ "Router" ] "toPath"
                , apply
                    [ fqFun [ "Router" ] "urlRoute"
                    , val "url"
                    ]
                    |> parens
                ]
                |> parens
            ]
        )


pageTypeDecl : List Module -> Declaration
pageTypeDecl pages =
    let
        pageConstructor module_ =
            ( pageName module_, [] )
    in
    pages
        |> List.map pageConstructor
        |> customTypeDecl Nothing "Page" []


fromRouteDecl : List Module -> Declaration
fromRouteDecl pages =
    let
        caseBranch page =
            ( namedPattern (routeName page) [], construct (pageName page) [] )
    in
    funDecl Nothing
        (Just
            (funAnn
                (fqTyped [ "Router" ] "Route" [])
                (typed "Page" [])
            )
        )
        "fromRoute"
        [ varPattern "route" ]
        (pages
            |> List.map caseBranch
            |> caseExpr (val "route")
        )


appUrlDecl : Declaration
appUrlDecl =
    aliasDecl Nothing
        "AppUrl"
        []
        (fqTyped [ "Router" ] "AppUrl" [])


appUrlsDecl : Declaration
appUrlsDecl =
    aliasDecl Nothing
        "AppUrls"
        []
        (fqTyped [ "Router" ] "AppUrls" [])


urlPageDecl : Declaration
urlPageDecl =
    funDecl Nothing
        (Just
            (funAnn
                (typed "AppUrl" [])
                (typed "Page" [])
            )
        )
        "urlPage"
        []
        (chain
            (fqFun [ "Router" ] "urlRoute")
            [ fun "fromRoute"
            ]
        )


urlStringDecl : Declaration
urlStringDecl =
    funDecl Nothing
        (Just
            (funAnn
                (typed "AppUrl" [])
                (typed "String" [])
            )
        )
        "urlString"
        []
        (chain
            (fqFun [ "Router" ] "urlRoute")
            [ fqFun [ "Router" ] "toPath"
            ]
        )



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
