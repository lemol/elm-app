module ElmApp exposing (..)

import ElmApp.Error exposing (Error)
import ElmApp.SingleModule as SingleModule


type App
    = SingleModuleApp SingleModule.Context


type alias AppContext =
    Result Error App


singleModule : String -> AppContext
singleModule source =
    SingleModule.parse source
        |> Result.map SingleModuleApp


write : AppContext -> Result Error (List ( String, String ))
write context =
    case context of
        Ok (SingleModuleApp x) ->
            SingleModule.write x
                |> Result.map List.singleton

        Err err ->
            Err err


process : AppContext -> Result Error (List ( String, String ))
process =
    write
