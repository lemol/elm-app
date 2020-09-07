port module Main exposing (main)

import ElmApp exposing (AppContext)
import ElmApp.Error as Error



-- PROGRAM


main : Program () Model Msg
main =
    Platform.worker
        { init = always init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



-- MESSAGES


type Msg
    = GotSourceCode ( String, String )
    | WriteApp AppContext



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSourceCode ( _, source ) ->
            let
                ctx =
                    ElmApp.singleModule source
            in
            update (WriteApp ctx) model

        WriteApp ctx ->
            let
                cmd =
                    case ElmApp.process ctx of
                        Ok x ->
                            writeResult x

                        Err e ->
                            printError (Error.toString e)
            in
            ( model
            , cmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ readSourceCode GotSourceCode
        ]



-- PORTS


port writeResult : List ( String, String ) -> Cmd msg


port printError : String -> Cmd msg


port readSourceCode : (( String, String ) -> msg) -> Sub msg
