module ElmApp.SingleModule exposing (..)

import Elm.CodeGen exposing (..)
import Elm.Pretty
import ElmApp.Error exposing (Error, notImplemented)
import ElmApp.Module exposing (Module)
import ElmApp.ModuleType exposing (ModuleType(..), detect)
import ElmApp.Parser as Parser
import ElmApp.SingleModule.ElementMainModule as ElementMainModule
import ElmApp.SingleModule.JustViewMainModule as JustViewMainModule
import ElmApp.SingleModule.SandboxMainModule as SandboxMainModule


type alias Context =
    Module


parse : String -> Result Error Context
parse =
    Parser.parseModule


write : Context -> Result Error ( String, String )
write context =
    let
        file_ =
            case detect context of
                JustView name ->
                    JustViewMainModule.write name context

                Sandbox ->
                    SandboxMainModule.write context

                Element ->
                    ElementMainModule.write context

                _ ->
                    Err notImplemented
    in
    file_
        |> Result.map (Elm.Pretty.pretty 120)
        |> Result.map (\code -> ( "Main.elm", code ))
