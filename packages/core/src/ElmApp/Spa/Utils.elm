module ElmApp.Spa.Utils exposing (..)

import Elm.CodeGen exposing (..)
import Elm.Syntax.Expression as Expression
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range



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


cmdNone : Expression
cmdNone =
    fqVal [ "Cmd" ] "none"



-- SUBSCRIPRIONS


subMsgGenericTA : TypeAnnotation
subMsgGenericTA =
    typed "Sub" [ typeVar "msg" ]


subMsgTA : TypeAnnotation
subMsgTA =
    typed "Sub" [ typeVar "Msg" ]


subCustomTA : TypeAnnotation -> TypeAnnotation
subCustomTA msg =
    typed "Sub" [ msg ]


subMap : String -> Expression -> Expression
subMap toMsg expr =
    apply
        [ fqFun [ "Sub" ] "map"
        , construct toMsg []
        , expr
        ]


subBatch : List Expression -> Expression
subBatch items =
    apply
        [ fqFun [ "Sub" ] "batch"
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



-- ELM.SYNTAX HELPERS


applySlash : Expression -> Expression -> Expression
applySlash exprl exprr =
    Expression.OperatorApplication "</>"
        Infix.Left
        (Node Range.emptyRange exprl)
        (Node Range.emptyRange exprr)
