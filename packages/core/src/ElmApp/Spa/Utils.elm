module ElmApp.Spa.Utils exposing (..)

import Elm.CodeGen exposing (..)



-- BASIC


modelTA : TypeAnnotation
modelTA =
    typed "Model" []


flagsTA : TypeAnnotation
flagsTA =
    typed "Flags" []


msgTA : TypeAnnotation
msgTA =
    typed "Msg" []



-- CMD


cmdMsgGenericTA : TypeAnnotation
cmdMsgGenericTA =
    typed "Cmd" [ typeVar "msg" ]


cmdMsgTA : TypeAnnotation
cmdMsgTA =
    typed "Cmd" [ typeVar "Msg" ]


cmdCustomTA : TypeAnnotation -> TypeAnnotation
cmdCustomTA msg =
    typed "Cmd" [ msg ]


cmdMap : String -> Expression -> Expression
cmdMap toMsg expr =
    apply
        [ fqFun [ "Cmd" ] "map"
        , construct toMsg []
        , expr
        ]


cmdBatch : List Expression -> Expression
cmdBatch items =
    apply
        [ fqFun [ "Cmd" ] "batch"
        , list items
        ]



-- MODEL


modelAccess : String -> Expression
modelAccess =
    access modelVal


modelVal : Expression
modelVal =
    val "model"


msgVal : Expression
msgVal =
    val "msg"



-- UPDATE


updateTA : TypeAnnotation
updateTA =
    funAnn msgTA
        (funAnn modelTA
            (tupleAnn
                [ modelTA
                , cmdMsgTA
                ]
            )
        )


updateDecl : Expression -> Declaration
updateDecl =
    funDecl Nothing
        (Just updateTA)
        "update"
        [ varPattern "msg"
        , varPattern "model"
        ]
