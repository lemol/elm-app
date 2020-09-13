module Spa.Route exposing (..)

import ElmApp.Module exposing (Init(..), Model(..), Msg(..), Subscriptions(..), Update(..), View(..))
import ElmApp.Spa.Route as Route exposing (ParamType(..), Part(..), Path(..), Route, layouts, part, stringParam)
import Expect
import Json.Decode as Decode
import Test exposing (..)


suite : Test
suite =
    describe "Spa"
        [ test "Route: decode root" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "path": "/",
                            "module": "Pages.Index"
                        }
                        """

                    expected =
                        { path = Root
                        , name = "Index"
                        , module_ = [ "Pages", "Index" ]
                        , layouts = []
                        }
                in
                json
                    |> expectDecodeRouteTo expected
        , test "Route: decode one level" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "path": "/about",
                            "module": "Pages.About"
                        }
                        """

                    expected =
                        { path = Parts [ part "about" ]
                        , name = "About"
                        , module_ = [ "Pages", "About" ]
                        , layouts = []
                        }
                in
                json
                    |> expectDecodeRouteTo expected
        , test "Route: decode multiple levels" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "path": "/path/with/multiple/levels",
                            "module": "Pages.Multiple.Levels"
                        }
                        """

                    expected =
                        { path =
                            Parts
                                [ part "path"
                                , part "with"
                                , part "multiple"
                                , part "levels"
                                ]
                        , name = "Multiple_Levels"
                        , module_ = [ "Pages", "Multiple", "Levels" ]
                        , layouts = []
                        }
                in
                json
                    |> expectDecodeRouteTo expected
        , test "Route: decode with params" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "path": "/users/:userId/relations/:friendId",
                            "module": "Pages.User.RelationDetails"
                        }
                        """

                    expected =
                        { path =
                            Parts
                                [ part "users"
                                , stringParam "userId"
                                , part "relations"
                                , stringParam "friendId"
                                ]
                        , name = "User_RelationDetails"
                        , module_ = [ "Pages", "User", "RelationDetails" ]
                        , layouts = []
                        }
                in
                json
                    |> expectDecodeRouteTo expected
        , test "Route: with custom name" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "path": "/about",
                            "name": "About_Custom_Route_Name",
                            "module": "Pages.About"
                        }
                        """

                    expected =
                        { path = Parts [ part "about" ]
                        , name = "About_Custom_Route_Name"
                        , module_ = [ "Pages", "About" ]
                        , layouts = []
                        }
                in
                json
                    |> expectDecodeRouteTo expected
        , test "Route: outside of pages dir" <|
            \_ ->
                let
                    json =
                        """
                        {
                            "path": "/about",
                            "module": "CustomDir.About"
                        }
                        """

                    expected =
                        { path = Parts [ part "about" ]
                        , name = "CustomDir_About"
                        , module_ = [ "CustomDir", "About" ]
                        , layouts = []
                        }
                in
                json
                    |> expectDecodeRouteTo expected
        ]


expectDecodeRouteTo :
    { path : Path
    , name : String
    , module_ : List String
    , layouts : List Route
    }
    -> String
    -> Expect.Expectation
expectDecodeRouteTo expected json =
    json
        |> Decode.decodeString Route.routeDecoder
        |> Result.map
            (Expect.all
                [ Route.path
                    >> Expect.equal expected.path
                , Route.name
                    >> Expect.equal expected.name
                , Route.module_
                    >> .name
                    >> Expect.equal expected.module_
                , Route.layouts
                    >> Expect.equal expected.layouts
                ]
            )
        |> (\result ->
                case result of
                    Ok x ->
                        x

                    Err e ->
                        Expect.fail (Decode.errorToString e)
           )
