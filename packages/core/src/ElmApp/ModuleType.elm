module ElmApp.ModuleType exposing (..)

import ElmApp.Error exposing (Error(..))
import ElmApp.Module
    exposing
        ( Init(..)
        , Model(..)
        , Module
        , Update(..)
        , View(..)
        )


type ModuleType
    = Unknown
    | JustView String
    | Sandbox
    | Element
    | Application


detect : Module -> ModuleType
detect module_ =
    if isElement module_ then
        Element

    else if isSandbox module_ then
        Sandbox

    else
        case isJustView module_ of
            Just name ->
                JustView name

            Nothing ->
                Unknown


isJustView : Module -> Maybe String
isJustView { view } =
    case view of
        View_Document name _ ->
            Just name

        _ ->
            Nothing


isSandbox : Module -> Bool
isSandbox { model, init, update, view } =
    let
        checkModel =
            True

        checkInit =
            case init of
                Init_Model _ _ ->
                    True

                _ ->
                    model == Model0 && init == Init0

        checkUpdate =
            case update of
                Update_Msg_Model_Model _ _ _ _ ->
                    True

                _ ->
                    False

        checkView =
            case view of
                View_Document _ _ ->
                    True

                View_Model_Document _ _ _ ->
                    True

                _ ->
                    False
    in
    checkModel
        && checkInit
        && checkUpdate
        && checkView


isElement : Module -> Bool
isElement { model, init, update, view } =
    let
        checkModel =
            True

        checkInit =
            case init of
                Init_ModelCmd _ _ ->
                    True

                _ ->
                    model == Model0 && init == Init0

        checkMsg =
            True

        checkUpdate =
            case update of
                Update_Msg_Model_ModelCmd _ _ _ _ ->
                    True

                _ ->
                    False

        checkView =
            case view of
                View_Document _ _ ->
                    True

                View_Model_Document _ _ _ ->
                    True

                _ ->
                    False

        checkSubscriptions =
            True
    in
    checkModel
        && checkInit
        && checkMsg
        && checkUpdate
        && checkView
        && checkSubscriptions


viewName : Module -> Result Error String
viewName { view } =
    case view of
        View0 ->
            Err (Error "View was not found")

        View_Document name _ ->
            Ok name

        View_Model_Document name _ _ ->
            Ok name

        View_Bag_Model_Document name _ _ _ ->
            Ok name
