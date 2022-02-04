module Js.DecoderExtra exposing (..)
import Json.Decode


firstCharDecoder : Json.Decode.Decoder Char
firstCharDecoder =
    Json.Decode.string
        |> Json.Decode.map (String.uncons >> Maybe.map Tuple.first >> Maybe.withDefault ' ')