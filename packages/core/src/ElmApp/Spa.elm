module ElmApp.Spa exposing
    ( Context
    , addPage
    , build
    , init
    , initDecodingConfig
    , withGlobalModule
    , withLoadingModule
    , withNotFoundModule
    , withUnauthorizedModule
    , write
    )

import Dict exposing (Dict)
import Elm.CodeGen exposing (..)
import Elm.Pretty
import ElmApp.Error as Error exposing (Error(..))
import ElmApp.Module exposing (Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmApp.Parser as Parser
import ElmApp.Spa.Config as Config exposing (Config)
import ElmApp.Spa.MainModule as MainModule
import ElmApp.Spa.PagesModule as PagesModule
import ElmApp.Spa.Route as Route
import ElmApp.Spa.RouterModule as RouterModule
import Json.Decode as Decode
import List.Extra
import Result.Extra


type alias Context =
    { config : Config
    , pages : Dict String Module
    , globalModule : Maybe Module
    , loadingModule : Maybe Module
    , notFoundModule : Maybe Module
    , unauthorizedModule : Maybe Module
    }


type ContextBuilder
    = ContextBuilder (Result Error Context)


internalInit : Config -> Context
internalInit config =
    { config = config
    , pages = Dict.empty
    , globalModule = Nothing
    , loadingModule = Nothing
    , notFoundModule = Nothing
    , unauthorizedModule = Nothing
    }


init : Config -> ContextBuilder
init =
    internalInit >> Ok >> ContextBuilder


initDecodingConfig : Decode.Value -> ContextBuilder
initDecodingConfig =
    Decode.decodeValue Config.configDecoder
        >> Result.mapError Error.DecodeError
        >> Result.map internalInit
        >> ContextBuilder


withModule : (Context -> Module -> Context) -> String -> ContextBuilder -> ContextBuilder
withModule setModule source (ContextBuilder builder) =
    Parser.parseModule source
        |> Result.map2
            (\context module_ ->
                setModule context module_
            )
            builder
        |> ContextBuilder


withGlobalModule : String -> ContextBuilder -> ContextBuilder
withGlobalModule =
    withModule
        (\context module_ ->
            { context | globalModule = Just module_ }
        )


withLoadingModule : String -> ContextBuilder -> ContextBuilder
withLoadingModule =
    withModule
        (\context module_ ->
            { context | loadingModule = Just module_ }
        )


withNotFoundModule : String -> ContextBuilder -> ContextBuilder
withNotFoundModule =
    withModule
        (\context module_ ->
            { context | notFoundModule = Just module_ }
        )


withUnauthorizedModule : String -> ContextBuilder -> ContextBuilder
withUnauthorizedModule =
    withModule
        (\context module_ ->
            { context | unauthorizedModule = Just module_ }
        )


addPage : String -> ContextBuilder -> ContextBuilder
addPage =
    withModule
        (\context module_ ->
            { context
                | pages =
                    Dict.insert
                        (String.join "." module_.name)
                        module_
                        context.pages
                , config =
                    context.config
                        |> Config.updateRouteModule module_.name module_
            }
        )


build : ContextBuilder -> Result Error Context
build (ContextBuilder builder) =
    builder
        |> Result.andThen
            (\context ->
                case contextErrors context of
                    Just xs ->
                        Err (Error xs)

                    Nothing ->
                        { config = context.config
                        , pages = context.pages
                        , globalModule = context.globalModule
                        , loadingModule = context.loadingModule
                        , notFoundModule = context.notFoundModule
                        , unauthorizedModule = context.unauthorizedModule
                        }
                            |> Ok
            )


contextErrors : Context -> Maybe String
contextErrors context =
    context.config.routes
        |> List.Extra.find
            (\route ->
                context.pages
                    |> Dict.values
                    |> List.any
                        (\page ->
                            page.name == (Route.module_ route |> .name)
                        )
                    |> not
            )
        |> Maybe.map
            (\route ->
                "'" ++ Route.name route ++ "' route has no corresponding page module"
            )


write : Context -> Result Error (List ( String, String ))
write context =
    let
        pagesModule =
            PagesModule.write
                { config = context.config
                , pages = context.pages |> Dict.values
                }

        mainModule =
            MainModule.write { config = context.config }

        routerModule =
            RouterModule.write
                { config = context.config
                , pages = context.pages |> Dict.values
                }
    in
    [ ( "App/Pages.elm", pagesModule )
    , ( "App/Main.elm", mainModule )
    , ( "App/Pages/Internal/Router.elm", routerModule )
    ]
        |> writeAll


writeAll : List ( String, Result Error File ) -> Result Error (List ( String, String ))
writeAll fs =
    fs
        |> List.map (\( fn, ff ) -> ff |> Result.map (Tuple.pair fn))
        -- List (Result Error (String, File))
        |> Result.Extra.combine
        |> Result.map (List.map <| Tuple.mapSecond (Elm.Pretty.pretty 120))
