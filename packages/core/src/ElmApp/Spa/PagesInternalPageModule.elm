module ElmApp.Spa.PagesInternalPageModule exposing (..)

import Elm.CodeGen exposing (..)
import ElmApp.Error exposing (Error)
import ElmApp.Module as Module exposing (Module)
import ElmApp.ModuleType exposing (ModuleType(..))
import ElmApp.Spa.Config exposing (Config)
import ElmApp.Spa.Route as Route


type alias Options =
    { config : Config
    , pages : List Module
    }


write : Options -> Result Error File
write opts =
    let
        moduleName =
            [ "App", "Pages", "Internal", "Page" ]

        file_ =
            file
                (normalModule moduleName exposing_)
                (importList opts)
                declarations
                Nothing

        exposing_ =
            [ openTypeExpose "PageLoaded"
            ]

        declarations =
            [ pageLoadedTypeDecl opts
            ]
    in
    Ok file_


importList : Options -> List Import
importList opts =
    opts.config.routes
        |> List.map (Route.module_ >> .name)
        |> List.map (\name -> importStmt name Nothing Nothing)


pageLoadedTypeDecl : Options -> Declaration
pageLoadedTypeDecl opts =
    let
        pageLoadedContructTA route =
            ( Route.name route
            , [ Module.modelAnn (Route.module_ route)
                    |> Maybe.withDefault unitAnn
              , unitAnn
              ]
            )
    in
    (( "None", [] )
        :: (opts.config.routes
                |> List.map pageLoadedContructTA
           )
    )
        |> customTypeDecl Nothing "PageLoaded" []
