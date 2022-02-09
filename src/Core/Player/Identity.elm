module Core.Player.Identity exposing (..)
import Core.Player.Id exposing (PlayerId)
import Json.Encode
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Core.Player.Id exposing (PlayerId)
import Js.DecoderExtra exposing (firstCharDecoder)


type alias PlayerIdentity =
    { id : PlayerId
    , icon : Char
    , name : String
    }


encode : PlayerIdentity -> Json.Encode.Value
encode identity =
    Json.Encode.object
        [ ( "icon", Json.Encode.string <| String.fromChar identity.icon )
        , ( "name", Json.Encode.string identity.name )
        , ( "id", Core.Player.Id.encode identity.id )
        ]


decoder : Json.Decode.Decoder PlayerIdentity
decoder =
    Json.Decode.succeed PlayerIdentity
        |> required "id" Core.Player.Id.decoder
        |> required "icon" firstCharDecoder
        |> required "name" Json.Decode.string