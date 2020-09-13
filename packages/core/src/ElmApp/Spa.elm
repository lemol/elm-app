module ElmApp.Spa exposing
    ( Context
    , addPage
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
import ElmApp.Module exposing (DocumentInfo, Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmApp.Parser as Parser
import ElmApp.Spa.MainModule as MainModule exposing (flagsDecl)
import ElmApp.Spa.PagesModule as PagesModule
import Result.Extra


type alias Context =
    { documentInfo : DocumentInfo
    , pages : Dict String Module
    , globalModule : Maybe Module
    , loadingModule : Maybe Module
    , notFoundModule : Maybe Module
    , unauthorizedModule : Maybe Module
    }


init : { documentInfo : DocumentInfo } -> Context
init { documentInfo } =
    { documentInfo = documentInfo
    , pages = Dict.empty
    , globalModule = Nothing
    , loadingModule = Nothing
    , notFoundModule = Nothing
    , unauthorizedModule = Nothing
    }


withModule : (Context -> Module -> Context) -> String -> Result Error Context -> Result Error Context
withModule setModule source context =
    Result.map2
        setModule
        context
        (Parser.parseModule source)


withGlobalModule : String -> Result Error Context -> Result Error Context
withGlobalModule =
    withModule
        (\context module_ ->
            { context | globalModule = Just module_ }
        )


withLoadingModule : String -> Result Error Context -> Result Error Context
withLoadingModule =
    withModule
        (\context module_ ->
            { context | loadingModule = Just module_ }
        )


withNotFoundModule : String -> Result Error Context -> Result Error Context
withNotFoundModule =
    withModule
        (\context module_ ->
            { context | notFoundModule = Just module_ }
        )


withUnauthorizedModule : String -> Result Error Context -> Result Error Context
withUnauthorizedModule =
    withModule
        (\context module_ ->
            { context | unauthorizedModule = Just module_ }
        )


addPage : String -> Result Error Context -> Result Error Context
addPage =
    withModule
        (\context module_ ->
            { context
                | pages =
                    Dict.insert
                        (String.join "." module_.name)
                        module_
                        context.pages
            }
        )


write : Context -> Result Error (List ( String, String ))
write context =
    let
        pagesModule =
            context.pages
                |> Dict.values
                |> PagesModule.write

        mainModule =
            MainModule.write { documentInfo = context.documentInfo }
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
        |> Result.map (List.map <| Tuple.mapSecond (Elm.Pretty.pretty 50))
