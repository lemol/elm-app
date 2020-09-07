module ElmApp.ModuleType exposing (..)

import ElmApp.Error exposing (Error(..))
import ElmApp.Module exposing (Module, View(..))


type ModuleType
    = Unknown
    | JustView String
    | Sandbox
    | Element
    | Application


detect : Module -> ModuleType
detect { view } =
    case [ view ] of
        [ View_Document name _ ] ->
            JustView name

        _ ->
            Unknown


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
