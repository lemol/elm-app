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
            , pageTypeDecls pages
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
        []
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


pageTypeDecls : List Module -> Declaration
pageTypeDecls pages =
    let
        pageConstructor module_ =
            ( pageName module_.name, [] )
    in
    pages
        |> List.map pageConstructor
        |> customTypeDecl Nothing "Page" []



-- UTILS


pageName : List String -> String
pageName moduleName =
    case moduleName of
        "Pages" :: xs ->
            String.join "" xs

        xs ->
            String.join "" xs
