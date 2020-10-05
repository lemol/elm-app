module ElmApp.Module exposing
    ( Init(..)
    , Model(..)
    , Module
    , Msg(..)
    , Name
    , Params(..)
    , Subscriptions(..)
    , Update(..)
    , View(..)
    , bagRecordExpr
    , build
    , encodeView
    , hasModel
    , hasParams
    , initApply
    , initApplyDestruct
    , initHasCmd
    , modelAnn
    , msgAnn
    , paramsAnn
    , paramsLetDeclaration
    , paramsRecordAnn
    , subApply
    , updateApplyDestruct
    , updateHasCmd
    , viewApply
    , viewBagItems
    , viewBagRecordExpr
    , viewHasBag
    , withImports
    , withInit
    , withModel
    , withMsg
    , withParams
    , withSubscriptions
    , withUpdate
    , withView
    )

import Elm.CodeGen exposing (Expression, Import, ModuleName, TypeAnnotation, access, apply, fqFun, fqTyped, letDestructuring, record, tuplePattern, unit, val, varPattern)
import Elm.Syntax.Expression exposing (Expression, LetDeclaration(..))
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import ElmCodeGenUtils exposing (recordDefinitionFields, recordDefinitionFieldsTA)
import Json.Encode exposing (Value)
import Maybe.Extra


type alias Module =
    { name : ModuleName
    , imports : List Import
    , model : Model
    , params : Params
    , init : Init
    , msg : Msg
    , update : Update
    , subscriptions : Subscriptions
    , view : View
    }


type alias Name =
    String


type Model
    = Model0 -- no Model
    | Model1 Name -- type Model


type Params
    = Params0 -- no Params
    | Params1 Name TypeAnnotation -- type alias Params = {..record..}


type Init
    = Init0 -- no init
    | Init_Model Name TypeAnnotation -- init : Model
    | Init_ModelCmd Name TypeAnnotation -- init : ( Model, Cmd Msg )
    | Init_Bag_Model Name TypeAnnotation TypeAnnotation -- init : Bag -> Model
    | Init_Bag_ModelCmd Name TypeAnnotation TypeAnnotation -- init : Bag -> ( Model, Cmd Msg )


type Msg
    = Msg0 -- no msg
    | Msg1 Name -- type Msg


type Update
    = Update0 -- no update
    | Update_Msg_Model_Model Name TypeAnnotation TypeAnnotation TypeAnnotation -- update : Msg -> Model -> Model
    | Update_Msg_Model_ModelCmd Name TypeAnnotation TypeAnnotation TypeAnnotation -- update : Msg -> Model -> ( Model, Cmd Msg )
    | Update_Msg_Model_ModelCmdExternal Name TypeAnnotation TypeAnnotation TypeAnnotation -- update : Msg -> Model -> ( Model, Cmd Msg, Cmd ExternalMsg )
    | Update_Bag_Msg_Model_Model Name TypeAnnotation TypeAnnotation TypeAnnotation TypeAnnotation -- update : Msg -> Model -> Model
    | Update_Bag_Msg_Model_ModelCmd Name TypeAnnotation TypeAnnotation TypeAnnotation TypeAnnotation -- update : Msg -> Model -> ( Model, Cmd Msg )
    | Update_Bag_Msg_Model_ModelCmdExternal Name TypeAnnotation TypeAnnotation TypeAnnotation TypeAnnotation -- update : Msg -> Model -> ( Model, Cmd Msg, Cmd ExternalMsg )


type Subscriptions
    = Subscriptions0 -- no subscriptions
    | Subscriptions_Sub Name TypeAnnotation -- subscriptions : Cmd msg
    | Subscriptions_Model_Sub Name TypeAnnotation TypeAnnotation -- subscriptions : Model -> Cmd msg
    | Subscriptions_Bag_Model_Sub Name TypeAnnotation TypeAnnotation TypeAnnotation -- subscriptions : Bag -> Model -> Cmd msg


type View
    = View0 -- no view
    | View_Document Name TypeAnnotation -- view : Html msg
    | View_Model_Document Name TypeAnnotation TypeAnnotation -- view : Model -> Html msg
    | View_Bag_Document Name TypeAnnotation TypeAnnotation -- view : Bag -> Html msg
    | View_Bag_Model_Document Name TypeAnnotation TypeAnnotation TypeAnnotation -- view : Bag -> Model -> Html msg


encodeView : View -> Value
encodeView v =
    case v of
        View_Document _ ta ->
            TypeAnnotation.encode ta

        _ ->
            Json.Encode.string "OK"


type BagItem
    = BagUrls
    | BagRouter



-- BUILDER


build : ModuleName -> Module
build name =
    { name = name
    , imports = []
    , model = Model0
    , params = Params0
    , init = Init0
    , msg = Msg0
    , update = Update0
    , subscriptions = Subscriptions0
    , view = View0
    }


withImports : List Import -> Module -> Module
withImports imports mod =
    { mod | imports = imports }


withModel : Model -> Module -> Module
withModel model mod =
    { mod | model = model }


withParams : Params -> Module -> Module
withParams params mod =
    { mod | params = params }


withInit : Init -> Module -> Module
withInit init mod =
    { mod | init = init }


withMsg : Msg -> Module -> Module
withMsg msg mod =
    { mod | msg = msg }


withUpdate : Update -> Module -> Module
withUpdate update mod =
    { mod | update = update }


withSubscriptions : Subscriptions -> Module -> Module
withSubscriptions subscriptions mod =
    { mod | subscriptions = subscriptions }


withView : View -> Module -> Module
withView view mod =
    { mod | view = view }



-- TYPEANNOTATIONS


hasModel : Module -> Bool
hasModel =
    modelAnn >> Maybe.Extra.isJust


hasParams : Module -> Bool
hasParams =
    paramsAnn >> Maybe.Extra.isJust


initHasCmd : Module -> Bool
initHasCmd module_ =
    case module_.init of
        Init_ModelCmd _ _ ->
            True

        Init_Bag_ModelCmd _ _ _ ->
            True

        _ ->
            False


updateHasCmd : Module -> Bool
updateHasCmd module_ =
    case module_.update of
        Update_Bag_Msg_Model_ModelCmd _ _ _ _ _ ->
            True

        Update_Bag_Msg_Model_ModelCmdExternal _ _ _ _ _ ->
            True

        Update_Msg_Model_ModelCmd _ _ _ _ ->
            True

        Update_Msg_Model_ModelCmdExternal _ _ _ _ ->
            True

        _ ->
            False


viewHasBag : Module -> Bool
viewHasBag module_ =
    case module_.view of
        View_Bag_Document _ _ _ ->
            True

        View_Bag_Model_Document _ _ _ _ ->
            True

        _ ->
            False


modelAnn : Module -> Maybe TypeAnnotation
modelAnn module_ =
    case module_.model of
        Model1 name ->
            fqTyped module_.name name []
                |> Just

        Model0 ->
            Nothing


paramsAnn : Module -> Maybe TypeAnnotation
paramsAnn module_ =
    case module_.params of
        Params1 name _ ->
            fqTyped module_.name name []
                |> Just

        Params0 ->
            Nothing


paramsRecordAnn : Module -> Maybe TypeAnnotation
paramsRecordAnn module_ =
    case module_.params of
        Params1 _ rec ->
            Just rec

        Params0 ->
            Nothing


msgAnn : Module -> Maybe TypeAnnotation
msgAnn module_ =
    case module_.msg of
        Msg1 name ->
            fqTyped module_.name name []
                |> Just

        Msg0 ->
            Nothing


initApply : Module -> Maybe Expression
initApply module_ =
    case module_.init of
        Init_Model name _ ->
            apply
                [ fqFun module_.name name
                ]
                |> Just

        Init_ModelCmd name _ ->
            apply
                [ fqFun module_.name name
                ]
                |> Just

        Init0 ->
            Just unit

        _ ->
            Nothing


initApplyDestruct :
    { model : String
    , cmd : String
    }
    -> Module
    -> Maybe LetDeclaration
initApplyDestruct opts module_ =
    case module_.init of
        Init_Model name _ ->
            letDestructuring (varPattern opts.model)
                (apply
                    [ fqFun module_.name name
                    ]
                )
                |> Just

        Init_ModelCmd name _ ->
            letDestructuring
                (tuplePattern
                    [ varPattern opts.model
                    , varPattern opts.cmd
                    ]
                )
                (apply
                    [ fqFun module_.name name
                    ]
                )
                |> Just

        Init0 ->
            Nothing

        _ ->
            Nothing


paramsLetDeclaration : Module -> Maybe LetDeclaration
paramsLetDeclaration module_ =
    case module_.params of
        Params1 name (TypeAnnotation.Record paramsRecordDefs) ->
            letDestructuring (varPattern name)
                (recordDefinitionFields paramsRecordDefs
                    |> List.map
                        (\field ->
                            ( field, access (val "params") field )
                        )
                    |> record
                )
                |> Just

        Params0 ->
            Nothing

        _ ->
            Nothing


updateApplyDestruct :
    { model : String
    , cmd : String
    , msgArg : String
    , modelArg : String
    }
    -> Module
    -> Maybe LetDeclaration
updateApplyDestruct opts module_ =
    case module_.update of
        Update_Msg_Model_Model name _ _ _ ->
            letDestructuring (varPattern opts.model)
                (apply
                    [ fqFun module_.name name
                    , val opts.msgArg
                    , val opts.modelArg
                    ]
                )
                |> Just

        Update_Msg_Model_ModelCmd name _ _ _ ->
            letDestructuring
                (tuplePattern
                    [ varPattern opts.model
                    , varPattern opts.cmd
                    ]
                )
                (apply
                    [ fqFun module_.name name
                    , val opts.msgArg
                    , val opts.modelArg
                    ]
                )
                |> Just

        Update0 ->
            Nothing

        _ ->
            Nothing


viewBagTA : Module -> Maybe TypeAnnotation
viewBagTA module_ =
    case module_.view of
        View_Bag_Document _ bagTa _ ->
            Just bagTa

        View_Bag_Model_Document _ bagTa _ _ ->
            Just bagTa

        _ ->
            Nothing


viewBagItems : Module -> List BagItem
viewBagItems =
    viewBagTA
        >> Maybe.andThen recordDefinitionFieldsTA
        >> Maybe.withDefault []
        >> List.filterMap bagItemFromString


bagRecordExpr :
    { urls : Expression
    , router : Expression
    }
    -> List BagItem
    -> Expression
bagRecordExpr opts items =
    items
        |> List.map
            (\item ->
                ( bagItemString item
                , case item of
                    BagUrls ->
                        opts.urls

                    BagRouter ->
                        opts.router
                )
            )
        |> record


viewBagRecordExpr :
    { urls : Expression
    , router : Expression
    }
    -> Module
    -> Expression
viewBagRecordExpr opts =
    viewBagItems >> bagRecordExpr opts


bagItemFromString : String -> Maybe BagItem
bagItemFromString str =
    case str of
        "urls" ->
            Just BagUrls

        "router" ->
            Just BagRouter

        _ ->
            Nothing


bagItemString : BagItem -> String
bagItemString bagItem =
    case bagItem of
        BagUrls ->
            "urls"

        BagRouter ->
            "router"


viewApply :
    { model : Expression
    , bag : Expression
    }
    -> Module
    -> Maybe Expression
viewApply opts module_ =
    case module_.view of
        View_Document name _ ->
            fqFun module_.name name
                |> Just

        View_Bag_Document name _ _ ->
            apply
                [ fqFun module_.name name
                , opts.bag
                ]
                |> Just

        View_Model_Document name _ _ ->
            apply
                [ fqFun module_.name name
                , opts.model
                ]
                |> Just

        View_Bag_Model_Document name _ _ _ ->
            apply
                [ fqFun module_.name name
                , opts.bag
                , opts.model
                ]
                |> Just

        View0 ->
            Nothing


subApply :
    { model : Expression
    , bag : Expression
    }
    -> Module
    -> Maybe Expression
subApply opts module_ =
    case module_.subscriptions of
        Subscriptions_Sub name _ ->
            fqFun module_.name name
                |> Just

        Subscriptions_Model_Sub name _ _ ->
            apply
                [ fqFun module_.name name
                , opts.model
                ]
                |> Just

        Subscriptions_Bag_Model_Sub name _ _ _ ->
            apply
                [ fqFun module_.name name
                , opts.bag
                , opts.model
                ]
                |> Just

        Subscriptions0 ->
            Nothing
