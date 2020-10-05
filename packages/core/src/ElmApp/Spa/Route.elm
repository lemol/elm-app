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
    , notFoundRoute
    , params
    , parsePath
    , part
    , path
    , routeDecoder
    , stringParam
    , valueName
    , withModule
    )

import ElmApp.Module as Module exposing (Module)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import String.Extra



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


notFoundRoute : Module -> Route
notFoundRoute module__ =
    create
        { path = Parts [ Part "404" ]
        , name = "NotFound"
        , module_ = module__
        , layouts = []
        }


path : Route -> Path
path (Route info) =
    info.path


name : Route -> String
name (Route info) =
    info.name


valueName : Route -> String
valueName (Route info) =
    String.Extra.decapitalize info.name


module_ : Route -> Module
module_ (Route info) =
    info.module_


layouts : Route -> List Route
layouts (Route info) =
    info.layouts


withModule : Module -> Route -> Route
withModule newModule (Route info) =
    Route { info | module_ = newModule }


params : Route -> List ( ParamType, String )
params (Route info) =
    case info.path of
        Root ->
            []

        Parts parts ->
            parts
                |> List.filterMap
                    (\x ->
                        case x of
                            Param StringParam paramName ->
                                Just ( StringParam, paramName )

                            Param IntParam paramName ->
                                Just ( IntParam, paramName )

                            _ ->
                                Nothing
                    )



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
            case xs |> String.fromList |> String.split ":" of
                [ paramName ] ->
                    Param StringParam paramName

                [ paramName, typeName ] ->
                    case String.toLower typeName of
                        "string" ->
                            Param StringParam paramName

                        "int" ->
                            Param IntParam paramName

                        imposibleShouldReturnError ->
                            Part ""

                imposibleForNow ->
                    Part ""

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
