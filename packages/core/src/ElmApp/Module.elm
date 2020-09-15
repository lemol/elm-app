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
    , build
    , encodeView
    , modelAnn
    , paramsAnn
    , withImports
    , withInit
    , withModel
    , withMsg
    , withParams
    , withSubscriptions
    , withUpdate
    , withView
    )

import Elm.CodeGen exposing (Import, ModuleName, TypeAnnotation, fqTyped)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Json.Encode exposing (Value)


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
    | View_Bag_Model_Document Name TypeAnnotation TypeAnnotation TypeAnnotation -- view : Bag -> Model -> Html msg


encodeView : View -> Value
encodeView v =
    case v of
        View_Document _ ta ->
            TypeAnnotation.encode ta

        _ ->
            Json.Encode.string "OK"



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
