module ElmApp.Error exposing (Error(..), notImplemented, toString)

import Parser


type Error
    = Error String
    | ElmParserError (List Parser.DeadEnd)


notImplemented : Error
notImplemented =
    Error "not implemented"


toString : Error -> String
toString err =
    case err of
        Error x ->
            x

        ElmParserError x ->
            Parser.deadEndsToString x
