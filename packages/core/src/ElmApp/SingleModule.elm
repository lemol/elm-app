module ElmApp.SingleModule exposing (..)

import ElmApp.Error exposing (Error)
import ElmApp.Module exposing (Module)
import ElmApp.Parser as Parser


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
    Ok ""
