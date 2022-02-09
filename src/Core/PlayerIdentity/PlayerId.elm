module Core.PlayerIdentity.PlayerId exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Random
import Uuid exposing (uuidGenerator)


type PlayerId
    = PlayerId String


empty : PlayerId
empty =
    PlayerId ""


decoder : Decoder PlayerId
decoder =
    Decode.string
        |> Decode.map (\id -> PlayerId id)


encode : PlayerId -> Json.Encode.Value
encode player =
    case player of
        PlayerId id ->
            Json.Encode.string id


generator : Random.Generator PlayerId
generator =
    uuidGenerator |> Random.map Uuid.toString |> Random.map PlayerId
