module ElmApp.Spa exposing
    ( Context
    , addPage
    , build
    , init
    , withGlobalModule
    , withLoadingModule
    , withNotFoundModule
    , withUnauthorizedModule
    , write
    )

import Dict exposing (Dict)
import Elm.CodeGen exposing (..)
import Elm.Pretty
import ElmApp.Error exposing (Error)
import ElmApp.Module exposing (Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmApp.Parser as Parser
import ElmApp.Spa.Config as Config exposing (Config)
import ElmApp.Spa.MainModule as MainModule
import ElmApp.Spa.PagesModule as PagesModule
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


init : { config : Config } -> ContextBuilder
init { config } =
    { config = config
    , pages = Dict.empty
    , globalModule = Nothing
    , loadingModule = Nothing
    , notFoundModule = Nothing
    , unauthorizedModule = Nothing
    }
        |> Ok
        |> ContextBuilder


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
            (\builderContext ->
                { config = builderContext.config
                , pages = builderContext.pages
                , globalModule = builderContext.globalModule
                , loadingModule = builderContext.loadingModule
                , notFoundModule = builderContext.notFoundModule
                , unauthorizedModule = builderContext.unauthorizedModule
                }
                    |> Ok
            )


write : Context -> Result Error (List ( String, String ))
write context =
    let
        pagesModule =
            context.pages
                |> Dict.values
                |> PagesModule.write

        mainModule =
            MainModule.write { config = context.config }
    in
    [ ( "App/Pages.elm", pagesModule )
    , ( "App/Main.elm", mainModule )
    ]
        |> writeAll


writeAll : List ( String, Result Error File ) -> Result Error (List ( String, String ))
writeAll fs =
    fs
        |> List.map (\( fn, ff ) -> ff |> Result.map (Tuple.pair fn))
        -- List (Result Error (String, File))
        |> Result.Extra.combine
        |> Result.map (List.map <| Tuple.mapSecond (Elm.Pretty.pretty 120))
