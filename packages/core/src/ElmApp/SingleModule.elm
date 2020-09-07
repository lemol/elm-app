module ElmApp.SingleModule exposing (..)

import Elm.CodeGen exposing (..)
import Elm.Pretty
import ElmApp.Error exposing (Error, notImplemented)
import ElmApp.Module exposing (Module)
import ElmApp.ModuleType as ModuleType exposing (ModuleType(..), detect)
import ElmApp.Parser as Parser
import ElmCodeGenUtils exposing (typedGeneric)


type alias Context =
    Module


parse : String -> Result Error Context
parse =
    Parser.parseModule


write : Context -> Result Error ( String, String )
write context =
    writeSingleModule context
        |> Result.map (\code -> ( "Main.elm", code ))


writeSingleModule : Module -> Result Error String
writeSingleModule mod =
    let
        file_ =
            case detect mod of
                JustView name ->
                    writeSigleModule name mod

                _ ->
                    Err notImplemented
    in
    file_
        |> Result.map (Elm.Pretty.pretty 120)


writeSigleModule : String -> Module -> Result Error File
writeSigleModule viewName mod =
    let
        file_ =
            file
                (normalModule [ "Main" ] [ funExpose "main" ])
                [ importStmt mod.name Nothing Nothing
                , importStmt [ "Html" ] Nothing (Just (exposeExplicit [ typeOrAliasExpose "Html" ]))
                ]
                [ mainDecl ]
                Nothing

        mainDecl =
            funDecl Nothing (Just (typedGeneric "Html" "msg")) "main" [] (fqFun mod.name viewName)
    in
    Ok file_
