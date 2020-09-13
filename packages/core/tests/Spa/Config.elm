module Spa.Config exposing (..)

import ElmApp.Module exposing (Init(..), Model(..), Msg(..), Subscriptions(..), Update(..), View(..))
import ElmApp.Spa.Config as Config exposing (DocumentInfo(..))
import ElmApp.Spa.Route exposing (ParamType(..), Part(..), Path(..), Route)
import Expect
import Json.Decode as Decode
import Test exposing (..)


suite : Test
suite =
    describe "Config"
        [ test "can decode default document" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "routes": []
                        }
                        """

                    expected =
                        { document = DocumentModule [ "Html" ]
                        , routes = []
                        }
                in
                json
                    |> expectDecodeConfigTo expected
        , test "can decode document with just specifying the module" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "document": "Shared.Document",
                            "routes": []
                        }
                        """

                    expected =
                        { document = DocumentModule [ "Shared", "Document" ]
                        , routes = []
                        }
                in
                json
                    |> expectDecodeConfigTo expected
        , test "can decode document with custom info" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "document": {
                                "module": "Shared.UI",
                                "type": "MyDocument",
                                "map": "mapMyDocument",
                                "toBrowser": "myDocumentToBrowserDocument"
                            },
                            "routes": []
                        }
                        """

                    expected =
                        { document =
                            DocumentModuleCustom [ "Shared", "UI" ]
                                "MyDocument"
                                "mapMyDocument"
                                "myDocumentToBrowserDocument"
                        , routes = []
                        }
                in
                json
                    |> expectDecodeConfigTo expected
        ]


expectDecodeConfigTo :
    { document : DocumentInfo
    , routes :
        List
            { path : Path
            , name : String
            , module_ : List String
            , layouts : List Route
            }
    }
    -> String
    -> Expect.Expectation
expectDecodeConfigTo expected json =
    json
        |> Decode.decodeString Config.configDecoder
        |> Result.map
            (Expect.all
                [ .document
                    >> Expect.equal expected.document
                ]
            )
        |> (\result ->
                case result of
                    Ok x ->
                        x

                    Err e ->
                        Expect.fail (Decode.errorToString e)
           )
