module ElmApp.SingleModule.JustViewMainModule exposing (..)

import Elm.CodeGen exposing (..)
import ElmApp.Error exposing (Error)
import ElmApp.Module exposing (Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmCodeGenUtils exposing (typedGeneric)


write : String -> Module -> Result Error File
write viewName mod =
    let
        file_ =
            file
                (normalModule [ "App", "Main" ] [ funExpose "main" ])
                [ importStmt mod.name Nothing Nothing
                , importStmt [ "Html" ] Nothing (Just (exposeExplicit [ typeOrAliasExpose "Html" ]))
                ]
                [ mainDecl ]
                Nothing

        mainDecl =
            funDecl Nothing (Just (typedGeneric "Html" "msg")) "main" [] (fqFun mod.name viewName)
    in
    Ok file_



{-
   module Main exposing (main)

   import Html exposing (Html)
   import Main


   main : Html msg
   main =
       Main.main
-}
