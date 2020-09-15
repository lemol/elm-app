module ElmCodeGenUtils exposing (functionDeclaration, functionExposed, functionExposedOneOf, moduleDeclarations, moduleDefinition, recordTypeAliasDeclaration, typeAliasDeclaration, typeExposed, typeSimple, typedConcreteSimple, typedGeneric)

import Elm.CodeGen exposing (..)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Node as Node
import Elm.Syntax.TypeAlias exposing (TypeAlias)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import List.Extra


{-|

    typedGeneric "Html" "msg" == Html msg

-}
typedGeneric : String -> String -> TypeAnnotation
typedGeneric type_ generic =
    typed type_ [ typeVar generic ]


typedConcreteSimple : String -> String -> TypeAnnotation
typedConcreteSimple type_ arg =
    typed type_ [ typed arg [] ]


typeSimple : String -> TypeAnnotation
typeSimple type_ =
    typed type_ []


moduleDefinition : File -> Module
moduleDefinition =
    .moduleDefinition >> Node.value



-- TYPES


typeExposed : String -> Exposing -> Bool
typeExposed name exp =
    case exp of
        All _ ->
            False

        Explicit xs ->
            xs
                |> List.map Node.value
                |> List.member (TypeOrAliasExpose name)



-- FUNCTION


functionExposed : String -> Exposing -> Bool
functionExposed name exp =
    case exp of
        All _ ->
            False

        Explicit xs ->
            xs
                |> List.map Node.value
                |> List.member (FunctionExpose name)


functionExposedOneOf : List String -> Exposing -> Maybe String
functionExposedOneOf names exp =
    names
        |> List.map (\x -> Tuple.pair x x)
        |> List.map (Tuple.mapSecond (\x -> functionExposed x exp))
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> List.head


moduleDeclarations : File -> List Declaration.Declaration
moduleDeclarations file =
    file.declarations
        |> List.map
            (\decl ->
                case decl of
                    DeclWithComment _ f ->
                        f ""

                    DeclNoComment x ->
                        x
            )


functionDeclarations : List Declaration.Declaration -> List Function
functionDeclarations declarations =
    declarations
        |> List.filterMap
            (\x ->
                case x of
                    Declaration.FunctionDeclaration f ->
                        Just f

                    _ ->
                        Nothing
            )


functionName : Function -> String
functionName =
    .declaration >> Node.value >> .name >> Node.value


functionDeclaration : String -> List Declaration.Declaration -> Maybe Function
functionDeclaration name declarations =
    declarations
        |> functionDeclarations
        |> List.Extra.find
            (\x ->
                functionName x == name
            )



-- TYPE ALIAS


typeAliasDeclarations : List Declaration.Declaration -> List TypeAlias
typeAliasDeclarations declarations =
    declarations
        |> List.filterMap
            (\x ->
                case x of
                    Declaration.AliasDeclaration y ->
                        Just y

                    _ ->
                        Nothing
            )


typeAliasDeclaration : String -> List Declaration.Declaration -> Maybe TypeAlias
typeAliasDeclaration name declarations =
    declarations
        |> typeAliasDeclarations
        |> List.Extra.find
            (\x ->
                Node.value x.name == name
            )


recordTypeAliasDeclaration : String -> List Declaration.Declaration -> Maybe TypeAnnotation
recordTypeAliasDeclaration name declarations =
    typeAliasDeclaration name declarations
        |> Maybe.map (.typeAnnotation >> Node.value)
        |> Maybe.andThen
            (\ann ->
                case ann of
                    TypeAnnotation.Record _ ->
                        Just ann

                    _ ->
                        Nothing
            )
