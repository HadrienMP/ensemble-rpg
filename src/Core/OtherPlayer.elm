module Core.OtherPlayer exposing (..)
import Core.PlayerId exposing (PlayerId)
import Core.Role exposing (Role)
import Json.Encode
import Core.RoleCard
import Core.Player exposing (Player)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Js.DecoderExtra exposing (firstCharDecoder)

type alias OtherPlayer = 
    { id : PlayerId
    , name: String
    , icon: Char
    , completedRoles: List Role
    }

fromPlayer : Player -> OtherPlayer
fromPlayer player =
    { id = player.id
    , name = player.name
    , icon = player.icon
    , completedRoles = Core.Player.completedRoleCards player |> List.map .role
    }

encode : OtherPlayer -> Json.Encode.Value
encode player =
    Json.Encode.object 
    [ ("id", Core.PlayerId.encode player.id)
    , ("name", Json.Encode.string player.name)
    , ("icon", Json.Encode.string <| String.fromChar player.icon)
    , ("completedRoles", Json.Encode.list Core.RoleCard.encode <| player.completedRoles)
    ]

decoder : Json.Decode.Decoder OtherPlayer
decoder = 
    Json.Decode.succeed OtherPlayer
        |> required "id" Core.PlayerId.decoder
        |> required "name" Json.Decode.string
        |> required "icon" firstCharDecoder
        |> required "completedRoles" (Json.Decode.list Core.RoleCard.decoder)