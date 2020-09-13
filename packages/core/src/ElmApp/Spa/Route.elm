module ElmApp.Spa.Route exposing
    ( ParamType(..)
    , Part(..)
    , Path(..)
    , Route
    , create
    , intParam
    , layouts
    , module_
    , name
    , parsePath
    , part
    , path
    , routeDecoder
    , stringParam
    )

import ElmApp.Module as Module exposing (Module)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)



-- TYPES


type ParamType
    = StringParam
    | IntParam


type Part
    = Part String
    | Param ParamType String


type Path
    = Root
    | Parts (List Part)


type alias Internal =
    { path : Path
    , name : String
    , module_ : Module
    , layouts : List Route
    }


type Route
    = Route Internal



-- ROUTE


create : Internal -> Route
create =
    Route


path : Route -> Path
path (Route info) =
    info.path


name : Route -> String
name (Route info) =
    info.name


module_ : Route -> Module
module_ (Route info) =
    info.module_


layouts : Route -> List Route
layouts (Route info) =
    info.layouts



-- PARTS


part : String -> Part
part =
    Part


stringParam : String -> Part
stringParam =
    Param StringParam


intParam : String -> Part
intParam =
    Param IntParam



-- DECODE


parsePath : String -> Path
parsePath str =
    if str == "/" then
        Root

    else
        str
            |> String.split "/"
            |> List.filter ((/=) "")
            |> List.map strToPart
            |> Parts


{-| Simple:

    {
        "path": "/",
        "module": "Pages.Index"
    }

    ==> Route
        { path = Root
        , name = "Index"
        , module\_ = Module { name = ["Pages", "Index"] }
        , layouts = []
        }

-}
routeDecoder : Decoder Route
routeDecoder =
    Decode.succeed
        (\path_ name_ moduleName ->
            { path = path_
            , name = name_
            , moduleName = moduleName
            }
        )
        |> required "path" Decode.string
        |> optional "name" (Decode.maybe Decode.string) Nothing
        |> required "module" Decode.string
        |> Decode.andThen
            (\res ->
                Decode.succeed <|
                    Route
                        { path = parsePath res.path
                        , name =
                            res.name
                                |> Maybe.withDefault (routeNameFromModule res.moduleName)
                        , module_ = Module.build (moduleNameFromString res.moduleName)
                        , layouts = []
                        }
            )



-- UTILS


strToPart : String -> Part
strToPart str =
    case String.toList str of
        ':' :: xs ->
            xs
                |> String.fromList
                |> Param StringParam

        xs ->
            xs
                |> String.fromList
                |> Part


routeNameFromModule : String -> String
routeNameFromModule str =
    let
        trimed =
            case str |> String.split "." of
                "Pages" :: xs ->
                    xs

                xs ->
                    xs
    in
    trimed |> String.join "_"


moduleNameFromString : String -> List String
moduleNameFromString =
    String.split "."
