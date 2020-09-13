module ElmApp.Spa.RouteCodeGen exposing (..)

import Elm.CodeGen exposing (..)
import Elm.Syntax.Expression exposing (Expression)
import ElmApp.Spa.Route as Route exposing (ParamType(..), Part(..), Path(..), Route)


routePath : Route -> Expression
routePath route =
    case Route.path route of
        Root ->
            string "/"

        Parts xs ->
            xs
                |> List.map partToPath
                |> List.intersperse (string "/")
                |> applyBin append (string "/")


routeContructTA : Route -> ( String, List TypeAnnotation )
routeContructTA route =
    ( Route.name route, paramsTA (Route.path route) )


partToPath : Part -> Expression
partToPath part =
    case part of
        Part str ->
            string str

        Param pname StringParam ->
            val pname

        Param pname IntParam ->
            apply
                [ fqFun [ "String" ] "fromInt"
                , val pname
                ]


paramsTA : Path -> List TypeAnnotation
paramsTA path =
    case path of
        Root ->
            []

        Parts parts ->
            parts
                |> List.filterMap
                    (\x ->
                        case x of
                            Param _ StringParam ->
                                Just stringAnn

                            Param _ IntParam ->
                                Just intAnn

                            _ ->
                                Nothing
                    )


applyBin : BinOp -> Expression -> List Expression -> Expression
applyBin op zero xs =
    List.foldl (\acc act -> applyBinOp acc op act) zero xs
