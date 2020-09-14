module ElmApp.Spa.RouteCodeGen exposing (..)

import Elm.CodeGen exposing (..)
import Elm.Syntax.Expression exposing (Expression)
import ElmApp.Spa.Route as Route exposing (ParamType(..), Part(..), Path(..), Route, params, stringParam)
import ElmApp.Spa.Utils exposing (applySlash)


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
    ( Route.name route, paramsTA route )


pageContructTA : Route -> ( String, List TypeAnnotation )
pageContructTA route =
    ( Route.name route, [] )


routeUnconstructPattern : Route -> Pattern
routeUnconstructPattern route =
    let
        paramToPattern ( _, paramName ) =
            varPattern paramName
    in
    Route.params route
        |> List.map paramToPattern
        |> namedPattern (Route.name route)


routeUrlParserPath : Route -> Expression
routeUrlParserPath route =
    case Route.path route of
        Root ->
            val "top"

        Parts (x :: xs) ->
            xs
                |> List.map partToUrlParserPath
                |> List.foldl (\acc act -> applySlash act acc) (partToUrlParserPath x)

        Parts [] ->
            apply
                [ fun "oneOf", list [] ]


routeMatch : Route -> Expression
routeMatch route =
    apply
        [ fun "map"
        , construct (Route.name route) []
        , routeUrlParserPath route
            |> parens
        ]


routeUrlRecordTA : Route -> ( String, TypeAnnotation )
routeUrlRecordTA route =
    let
        urlParam params =
            case params of
                [] ->
                    typed "AppUrl" []

                x :: xs ->
                    funAnn x (urlParam xs)
    in
    ( Route.valueName route
    , Route.params route
        |> List.map
            (\param ->
                case Tuple.first param of
                    StringParam ->
                        stringAnn

                    IntParam ->
                        intAnn
            )
        |> urlParam
    )


routeUrlRecord : Route -> ( String, Expression )
routeUrlRecord route =
    let
        urlCall =
            construct "AppUrl"
                [ val "navigationKey"
                , Route.params route
                    |> List.map Tuple.second
                    |> List.map val
                    |> construct (Route.name route)
                    |> (if 0 == List.length (Route.params route) then
                            identity

                        else
                            parens
                       )
                ]

        urlDef params =
            case params of
                [] ->
                    urlCall

                xs ->
                    lambda
                        (xs
                            |> List.map Tuple.second
                            |> List.map varPattern
                        )
                        urlCall
    in
    ( Route.valueName route
    , Route.params route
        |> urlDef
    )


partToPath : Part -> Expression
partToPath part =
    case part of
        Part str ->
            string str

        Param StringParam paramName ->
            val paramName

        Param IntParam paramName ->
            apply
                [ fqFun [ "String" ] "fromInt"
                , val paramName
                ]


partToUrlParserPath : Part -> Expression
partToUrlParserPath part =
    case part of
        Part str ->
            apply
                [ fun "s"
                , string str
                ]

        Param StringParam _ ->
            fun "string"

        Param IntParam _ ->
            fun "int"


paramsTA : Route -> List TypeAnnotation
paramsTA route =
    let
        paramToAnn ( paramType, _ ) =
            case paramType of
                StringParam ->
                    stringAnn

                IntParam ->
                    intAnn
    in
    Route.params route
        |> List.map paramToAnn


applyBin : BinOp -> Expression -> List Expression -> Expression
applyBin op zero xs =
    List.foldl (\acc act -> applyBinOp act op acc) zero xs
