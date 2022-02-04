module Core.PlayerId exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Random
import Random.Char as RC
import Random.String as RS


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
    [ RC.geometricShape, RC.egyptianHieroglyph, RC.alchemicalSymbol ]
        |> List.map (RS.string 3)
        |> List.foldr (Random.map2 (++)) (Random.constant "")
        |> Random.map (\uuid -> PlayerId uuid)
