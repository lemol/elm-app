module ElmApp.Parser exposing (parseModule)

import Elm.CodeGen exposing (File)
import Elm.DSLParser
import Elm.Syntax.Module as ESModule
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..))
import ElmApp.Error exposing (Error(..))
import ElmApp.Module as Module exposing (Init(..), Model(..), Module, Msg(..), Subscriptions(..), Update(..), View(..))
import ElmCodeGenUtils exposing (functionDeclaration, functionExposed, functionExposedOneOf, moduleDeclarations, moduleDefinition, typeExposed)
import Maybe.Extra
import Parser



-- CONTEXT


type alias ParserContext =
    { result : Module
    , file : File
    }


type alias ParserResult =
    Result Error ParserContext


initContext : Result (List Parser.DeadEnd) File -> ParserResult
initContext fileResult =
    fileResult
        |> Result.map
            (\file ->
                let
                    moduleName =
                        file.moduleDefinition
                            |> Node.value
                            |> ESModule.moduleName
                in
                { result = Module.build moduleName
                , file = file
                }
            )
        |> Result.mapError ElmParserError


andThen : (ParserContext -> Result Error Module) -> ParserResult -> ParserResult
andThen next contextResult =
    contextResult
        |> Result.andThen
            (\context ->
                next context
                    |> Result.map (\result -> { context | result = result })
            )


moduleResult : ParserResult -> Result Error Module
moduleResult =
    Result.map .result



-- PARSE


parseModule : String -> Result Error Module
parseModule source =
    Elm.DSLParser.parse source
        |> initContext
        |> andThen parseModel
        |> andThen parseInit
        |> andThen parseMsg
        |> andThen parseUpdate
        |> andThen parseSubscriptions
        |> andThen parseView
        |> moduleResult



-- MODEL


parseModel : ParserContext -> Result Error Module
parseModel { file, result } =
    let
        exposed =
            file
                |> moduleDefinition
                |> ESModule.exposingList
                |> typeExposed "Model"

        model =
            if exposed then
                Model1 "Model"

            else
                Model0
    in
    result
        |> Module.withModel model
        |> Ok



-- INIT


parseInit : ParserContext -> Result Error Module
parseInit { file, result } =
    let
        exposed =
            file
                |> moduleDefinition
                |> ESModule.exposingList
                |> functionExposed "init"

        declaration =
            file
                |> moduleDeclarations
                |> functionDeclaration "init"
                |> Maybe.map .signature
                |> Maybe.Extra.join
                |> Maybe.map
                    (Node.value
                        >> .typeAnnotation
                        >> Node.value
                    )
                |> Maybe.map (fromInitDeclaration "init")
                |> Maybe.withDefault Init0

        init =
            if exposed then
                declaration

            else
                Init0
    in
    result
        |> Module.withInit init
        |> Ok


fromInitDeclaration : String -> TypeAnnotation -> Init
fromInitDeclaration name ann =
    case ann of
        Typed x vars ->
            Init_Model name (Typed x vars)

        Tupled [ model, msg ] ->
            Init_ModelCmd name (Tupled [ model, msg ])

        _ ->
            Init0



-- MSG


parseMsg : ParserContext -> Result Error Module
parseMsg { file, result } =
    let
        exposed =
            file
                |> moduleDefinition
                |> ESModule.exposingList
                |> typeExposed "Msg"

        found =
            if exposed then
                Msg1 "Msg"

            else
                Msg0
    in
    result
        |> Module.withMsg found
        |> Ok



-- UPDATE


parseUpdate : ParserContext -> Result Error Module
parseUpdate { file, result } =
    let
        exposed =
            file
                |> moduleDefinition
                |> ESModule.exposingList
                |> functionExposed "update"

        declaration =
            file
                |> moduleDeclarations
                |> functionDeclaration "update"
                |> Maybe.map .signature
                |> Maybe.Extra.join
                |> Maybe.map
                    (Node.value
                        >> .typeAnnotation
                        >> Node.value
                    )
                |> Maybe.map (fromUpdateDeclaration "update")
                |> Maybe.withDefault Update0

        found =
            if exposed then
                declaration

            else
                Update0
    in
    result
        |> Module.withUpdate found
        |> Ok


fromUpdateDeclaration : String -> TypeAnnotation -> Update
fromUpdateDeclaration name ann =
    case ann of
        FunctionTypeAnnotation msg (Node _ (FunctionTypeAnnotation model model2)) ->
            Update_Msg_Model_Model name (Node.value msg) (Node.value model) (Node.value model2)

        _ ->
            Update0



-- SUBSCRIPTIONS


parseSubscriptions : ParserContext -> Result Error Module
parseSubscriptions { file, result } =
    let
        exposed =
            file
                |> moduleDefinition
                |> ESModule.exposingList
                |> functionExposed "subscriptions"

        declaration =
            file
                |> moduleDeclarations
                |> functionDeclaration "subscriptions"
                |> Maybe.map .signature
                |> Maybe.Extra.join
                |> Maybe.map
                    (Node.value
                        >> .typeAnnotation
                        >> Node.value
                    )
                |> Maybe.map (fromSubscriptionsDeclaration "subscriptions")
                |> Maybe.withDefault Subscriptions0

        found =
            if exposed then
                declaration

            else
                Subscriptions0
    in
    result
        |> Module.withSubscriptions found
        |> Ok


fromSubscriptionsDeclaration : String -> TypeAnnotation -> Subscriptions
fromSubscriptionsDeclaration name ann =
    case ann of
        Typed doc vars ->
            Subscriptions_Sub name (Typed doc vars)

        FunctionTypeAnnotation model (Node _ (Typed doc vars)) ->
            Subscriptions_Model_Sub name (Node.value model) (Typed doc vars)

        _ ->
            Subscriptions0



-- VIEW


parseView : ParserContext -> Result Error Module
parseView { file, result } =
    let
        exposed =
            file
                |> moduleDefinition
                |> ESModule.exposingList
                |> functionExposedOneOf [ "main", "view" ]

        declaration name =
            file
                |> moduleDeclarations
                |> functionDeclaration name
                |> Maybe.map .signature
                |> Maybe.Extra.join
                |> Maybe.map
                    (Node.value
                        >> .typeAnnotation
                        >> Node.value
                    )
                |> Maybe.map (fromViewDeclaration name)
                |> Maybe.withDefault View0

        found =
            exposed
                |> Maybe.map declaration
                |> Maybe.withDefault View0
    in
    result
        |> Module.withView found
        |> Ok


fromViewDeclaration : String -> TypeAnnotation -> View
fromViewDeclaration name ann =
    case ann of
        Typed doc vars ->
            View_Document name (Typed doc vars)

        FunctionTypeAnnotation model (Node _ (Typed doc vars)) ->
            View_Model_Document name (Node.value model) (Typed doc vars)

        _ ->
            View0
