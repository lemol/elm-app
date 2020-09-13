module ElmApp.Spa.Config exposing
    ( Config
    , DocumentInfo(..)
    , configDecoder
    , default
    , updateRouteModule
    )

import ElmApp.Module exposing (Module)
import ElmApp.Spa.Route as Route exposing (Route, routeDecoder)
import Json.Decode as Decode exposing (Decoder, oneOf)
import Json.Decode.Pipeline exposing (optional, required)
import List.Extra


type alias Config =
    { routes : List Route
    , document : DocumentInfo
    }


type DocumentInfo
    = DocumentModule (List String)
    | DocumentModuleCustom (List String) String String String


default : Config
default =
    { routes = []
    , document = defaultDocument
    }


updateRouteModule : List String -> Module -> Config -> Config
updateRouteModule routeModuleName module_ config =
    { config
        | routes =
            config.routes
                |> List.Extra.updateIf
                    (\route ->
                        routeModuleName
                            == (route
                                    |> Route.module_
                                    |> .name
                               )
                    )
                    (Route.withModule module_)
    }



-- SERIALIZATION


configDecoder : Decoder Config
configDecoder =
    Decode.succeed Config
        |> required "routes" (Decode.list routeDecoder)
        |> optional "document" documentInfoDecoder defaultDocument


documentInfoDecoder : Decoder DocumentInfo
documentInfoDecoder =
    oneOf
        [ Decode.string
            |> Decode.map
                (\documentModule ->
                    documentModule
                        |> String.split "."
                        |> DocumentModule
                )
        , Decode.succeed
            (\moduleName documentType mapDocument toBrowser ->
                { moduleName = moduleName
                , documentType = documentType
                , mapDocument = mapDocument
                , toBrowser = toBrowser
                }
            )
            |> required "module" Decode.string
            |> optional "type" Decode.string "Document"
            |> optional "map" Decode.string "map"
            |> optional "toBrowser" Decode.string "toBrowser"
            |> Decode.map
                (\x ->
                    DocumentModuleCustom (x.moduleName |> String.split ".")
                        x.documentType
                        x.mapDocument
                        x.toBrowser
                )
        ]


defaultDocument : DocumentInfo
defaultDocument =
    DocumentModule [ "Html" ]
