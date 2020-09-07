module Global exposing (Model, Msg(..), init, update)

-- MODEL


type alias Model =
    Maybe String


init : ( Model, Cmd Msg )
init =
    ( Nothing
    , Cmd.none
    )



-- MESSAGES


type Msg
    = Login String
    | Logout



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        Login username ->
            ( Just username, Cmd.none )

        Logout ->
            ( Nothing, Cmd.none )
