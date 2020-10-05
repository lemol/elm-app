module ElmApp.Spa.Writers.Internal.Pages_Internal_Main_Elm exposing (..)

import Elm.CodeGen exposing (..)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Pattern exposing (Pattern)
import ElmApp.Module as Module
import ElmApp.Spa.Config as Config exposing (Config)
import ElmApp.Spa.Route as Route exposing (Route)
import ElmApp.Spa.Utils exposing (cmdMap, cmdNone, modelVal, subMap)



{-
      ( CounterMsg subMsg, InternalPage.Counter subModel_prev _ ) ->
          let
              ( subModel, subCmd ) =
                  Pages.Counter.update subMsg subModel_prev

              current =
                  InternalPage.Counter subModel ()
          in
          ( updateCurrent current model
          , Cmd.map CounterMsg subCmd
          , Cmd.none
          )


   https://www.tiempodev.com/blog/what-are-the-advantages-of-agile-in-financial-services-software/
   https://financeandriskblog.accenture.com/finance-accounting/the-benefits-of-agile-platforms-in-financial-services
   https://www.raconteur.net/business-innovation/5-reasons-why-agility-fails-in-financial-services

   https://medium.com/devops-cloud-it-career/azure-devops-or-github-c83fe1eced4d
   https://acloudguru.com/blog/engineering/azure-devops-vs-github-comparing-microsofts-devops-twins

-}


updateFunCaseBranch : Route -> Maybe ( Pattern, Expression )
updateFunCaseBranch route =
    let
        module_ =
            Route.module_ route

        subModel_prev_pattern =
            varPattern "subModel_prev"

        paramsPattern =
            allPattern

        currentLet =
            pageLoadedConstruct
                { model = val "subModel"
                , params = unit
                }
                route

        subUpdate =
            Module.updateApplyDestruct
                { model = "subModel"
                , cmd = "subCmd"
                , modelArg = "subModel_prev"
                , msgArg = "subMsg"
                }
                module_

        letDecls =
            [ subUpdate
            , letVal "current" currentLet
                |> Just
            ]
                |> List.filterMap identity

        result =
            tuple
                [ apply
                    [ fun "updateCurrent"
                    , val "current"
                    , modelVal
                    ]
                , if Module.updateHasCmd module_ then
                    cmdMap (pageMsg route) (val "subCmd")

                  else
                    cmdNone
                , cmdNone
                ]
    in
    Module.msgAnn module_
        |> Maybe.map
            (( tuplePattern
                [ pageMsgDestructPattern
                    { msg = varPattern "subMsg" }
                    route
                , pageLoadedDestructPattern
                    { model = subModel_prev_pattern
                    , params = paramsPattern
                    }
                    route
                ]
             , letExpr letDecls result
             )
                |> always
            )


viewFunCaseBranch : Config -> Route -> Maybe ( Pattern, Expression )
viewFunCaseBranch config route =
    let
        module_ =
            Route.module_ route

        subModelPattern =
            if Module.hasModel module_ then
                varPattern "subModel"

            else
                allPattern

        paramsPattern =
            if Module.hasParams module_ then
                varPattern "params"

            else
                allPattern

        subBag =
            Module.viewBagRecordExpr
                { urls =
                    apply
                        [ fqFun [ "Router" ] "urls"
                        , access (access (val "bag") "router") "navigationKey"
                        ]
                , router =
                    access (val "bag") "router"
                }
                module_

        subView =
            Module.viewApply
                { bag = val "subBag"
                , model = val "subModel"
                }
                module_

        letDecls =
            [ if Module.viewHasBag module_ then
                letVal "subBag" subBag
                    |> Just

              else
                Nothing
            , subView
                |> Maybe.map (letVal "subView")
            ]
                |> List.filterMap identity

        result =
            pipe (val "subView")
                [ apply
                    [ case Config.documentMap config.document of
                        ( [], mapName ) ->
                            fun mapName

                        ( modName, mapName ) ->
                            fqFun modName mapName
                    , pageMsg route |> val
                    ]
                ]
    in
    Just
        ( pageLoadedDestructPattern
            { model = subModelPattern
            , params = paramsPattern
            }
            route
        , letExpr letDecls result
        )


subFunCaseBranch : Config -> Route -> Maybe ( Pattern, Expression )
subFunCaseBranch config route =
    let
        module_ =
            Route.module_ route

        subModelPattern =
            if Module.hasModel module_ then
                varPattern "subModel"

            else
                allPattern

        paramsPattern =
            if Module.hasParams module_ then
                varPattern "params"

            else
                allPattern

        subSub =
            Module.subApply
                { model = val "subModel"
                , bag = unit
                }
                module_

        letDecls =
            [ subSub
                |> Maybe.map (letVal "subSub")
            ]
                |> List.filterMap identity

        result =
            subMap (pageMsg route) (val "subSub")
    in
    subSub
        |> Maybe.map
            (( pageLoadedDestructPattern
                { model = subModelPattern
                , params = paramsPattern
                }
                route
             , letExpr letDecls result
             )
                |> always
            )



-- UTILS


pageMsg : Route -> String
pageMsg route =
    Route.name route ++ "Msg"


pageMsgDestructPattern : { msg : Pattern } -> Route -> Pattern
pageMsgDestructPattern opts route =
    namedPattern (pageMsg route) [ opts.msg ]


pageLoadedDestructPattern :
    { model : Pattern
    , params : Pattern
    }
    -> Route
    -> Pattern
pageLoadedDestructPattern opts route =
    fqNamedPattern [ "InternalPage" ]
        (Route.name route)
        [ opts.model, opts.params ]


pageLoadedDestruct :
    { model : Expression
    , params : Expression
    }
    -> Route
    -> Expression
pageLoadedDestruct opts route =
    fqConstruct [ "InternalPage" ]
        (Route.name route)
        [ opts.model, opts.params ]


pageLoadedConstruct :
    { model : Expression
    , params : Expression
    }
    -> Route
    -> Expression
pageLoadedConstruct opts route =
    fqConstruct [ "InternalPage" ]
        (Route.name route)
        [ opts.model, opts.params ]
