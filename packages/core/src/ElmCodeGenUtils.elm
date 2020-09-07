module ElmCodeGenUtils exposing (functionDeclaration, functionExposed, functionExposedOneOf, moduleDeclarations, moduleDefinition, typeExposed, typeSimple, typedConcreteSimple, typedGeneric)

import Elm.CodeGen exposing (..)
import Elm.Syntax.Declaration as Declaration
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Node as Node
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
