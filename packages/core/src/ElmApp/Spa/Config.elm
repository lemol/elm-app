module ElmApp.Spa.Config exposing (Config, configDecoder)

import ElmApp.Spa.Route exposing (Route, routeDecoder)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Config =
    { routes : List Route
    }



-- SERIALIZATION


configDecoder : Decoder Config
configDecoder =
    Decode.succeed Config
        |> required "routes" (Decode.list routeDecoder)
