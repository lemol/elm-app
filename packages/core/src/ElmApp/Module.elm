module ElmApp.Module exposing
    ( DocumentInfo(..)
    , Init(..)
    , Model(..)
    , Module
    , Msg(..)
    , Name
    , Subscriptions(..)
    , Update(..)
    , View(..)
    , build
    , encodeView
    , withImports
    , withInit
    , withModel
    , withMsg
    , withSubscriptions
    , withUpdate
    , withView
    )

import Elm.CodeGen exposing (Import, ModuleName, TypeAnnotation)
import Elm.Syntax.TypeAnnotation as TypeAnnotation
import Json.Encode exposing (Value)


type alias Module =
    { name : ModuleName
    , imports : List Import
    , model : Model
    , init : Init
    , msg : Msg
    , update : Update
    , subscriptions : Subscriptions
    , view : View
    }


type DocumentInfo
    = DocumentModule (List String)
    | DocumentModuleCustom (List String) String String


type alias Name =
    String


type Model
    = Model0 -- no Model
    | Model1 Name -- type Model


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
