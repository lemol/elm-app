module ElmApp.SingleModule.SandboxMainModule exposing (..)

import Elm.CodeGen exposing (..)
import ElmApp.Error exposing (Error)
import ElmApp.Module exposing (Module)
import ElmApp.ModuleType exposing (ModuleType(..))


write : Module -> Result Error File
write module_ =
    let
        moduleName =
            module_.name

        file_ =
            file
                (normalModule [ "App", "Main" ] [ funExpose "main" ])
                [ importStmt moduleName Nothing Nothing
                , importStmt [ "Browser" ] Nothing Nothing
                ]
                [ mainDecl ]
                Nothing

        mainDecl =
            funDecl Nothing
                (Just
                    (typed "Program"
                        [ unitAnn
                        , fqTyped moduleName "Model" []
                        , fqTyped moduleName "Msg" []
                        ]
                    )
                )
                "main"
                []
                (apply
                    [ fqFun [ "Browser" ] "sandbox"
                    , record
                        [ ( "init", fqFun moduleName "init" )
                        , ( "update", fqFun moduleName "update" )
                        , ( "view", fqFun moduleName "view" )
                        ]
                    ]
                )
    in
    Ok file_



{-  -}
