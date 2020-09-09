module Utils exposing (clearModuleNodeRange, clearViewNodeRange)

import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range
import Elm.Syntax.TypeAnnotation exposing (..)
import ElmApp.Module exposing (Init(..), Module, Subscriptions(..), Update(..), View(..))


clearModuleNodeRange : Module -> Module
clearModuleNodeRange mod =
    { name = mod.name
    , imports = mod.imports -- |> clearImportsNodeRange
    , model = mod.model -- |> clearModelNodeRange
    , init = mod.init |> clearInitNodeRange
    , msg = mod.msg -- |> clearMsgNodeRange
    , update = mod.update |> clearUpdateNodeRange
    , subscriptions = mod.subscriptions |> clearSubscriptionsNodeRange
    , view = mod.view |> clearViewNodeRange
    }


clearInitNodeRange : Init -> Init
clearInitNodeRange init =
    case init of
        Init0 ->
            init

        Init_Model n ta ->
            Init_Model n (clearTypeAnnotationNodeRange ta)

        Init_ModelCmd n ta ->
            Init_ModelCmd n (clearTypeAnnotationNodeRange ta)

        Init_Bag_Model n ta1 ta2 ->
            Init_Bag_Model n (clearTypeAnnotationNodeRange ta1) (clearTypeAnnotationNodeRange ta2)

        Init_Bag_ModelCmd n ta1 ta2 ->
            Init_Bag_ModelCmd n (clearTypeAnnotationNodeRange ta1) (clearTypeAnnotationNodeRange ta2)


clearUpdateNodeRange : Update -> Update
clearUpdateNodeRange update =
    case update of
        Update0 ->
            update

        Update_Msg_Model_Model n ta1 ta2 ta3 ->
            Update_Msg_Model_Model n (clearTypeAnnotationNodeRange ta1) (clearTypeAnnotationNodeRange ta2) (clearTypeAnnotationNodeRange ta3)

        Update_Msg_Model_ModelCmd n ta1 ta2 ta3 ->
            Update_Msg_Model_ModelCmd n (clearTypeAnnotationNodeRange ta1) (clearTypeAnnotationNodeRange ta2) (clearTypeAnnotationNodeRange ta3)

        _ ->
            update


clearViewNodeRange : View -> View
clearViewNodeRange view =
    case view of
        View0 ->
            view

        View_Document n ta ->
            View_Document n (clearTypeAnnotationNodeRange ta)

        View_Model_Document n ta1 ta2 ->
            View_Model_Document n (clearTypeAnnotationNodeRange ta1) (clearTypeAnnotationNodeRange ta2)

        View_Bag_Model_Document n ta1 ta2 ta3 ->
            View_Bag_Model_Document n (clearTypeAnnotationNodeRange ta1) (clearTypeAnnotationNodeRange ta2) (clearTypeAnnotationNodeRange ta3)


clearSubscriptionsNodeRange : Subscriptions -> Subscriptions
clearSubscriptionsNodeRange subscriptions =
    case subscriptions of
        Subscriptions0 ->
            subscriptions

        Subscriptions_Sub n ta ->
            Subscriptions_Sub n (clearTypeAnnotationNodeRange ta)

        Subscriptions_Model_Sub n ta1 ta2 ->
            Subscriptions_Model_Sub n (clearTypeAnnotationNodeRange ta1) (clearTypeAnnotationNodeRange ta2)

        Subscriptions_Bag_Model_Sub n ta1 ta2 ta3 ->
            Subscriptions_Bag_Model_Sub n (clearTypeAnnotationNodeRange ta1) (clearTypeAnnotationNodeRange ta2) (clearTypeAnnotationNodeRange ta3)


clearTypeAnnotationNodeRange : TypeAnnotation -> TypeAnnotation
clearTypeAnnotationNodeRange ta =
    case ta of
        Typed x list ->
            Typed (clearNodeRange x) (List.map (clearNodeRange >> Node.map clearTypeAnnotationNodeRange) list)

        Tupled list ->
            Tupled (List.map (clearNodeRange >> Node.map clearTypeAnnotationNodeRange) list)

        _ ->
            ta


clearNodeRange : Node a -> Node a
clearNodeRange (Node _ a) =
    Node Range.emptyRange a
