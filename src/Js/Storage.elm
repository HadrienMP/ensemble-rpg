port module Js.Storage exposing (..)

import Core.Player.Identity exposing (PlayerIdentity)
import Json.Decode
import Json.Decode.Pipeline exposing (optional)
import Json.Encode


port saveInStorage : Json.Encode.Value -> Cmd msg


type alias Storage =
    { player : Maybe PlayerIdentity
    }


saveIdentity : PlayerIdentity -> Cmd msg
saveIdentity identity =
    persist { player = Just identity }


persist : Storage -> Cmd msg
persist storage =
    saveInStorage <|
        case storage.player of
            Nothing ->
                Json.Encode.object [ ( "player", Json.Encode.null ) ]

            Just player ->
                Json.Encode.object
                    [ ( "player"
                      , Json.Encode.object
                            [ ( "identity", Core.Player.Identity.encode player )
                            ]
                      )
                    ]


fromString : String -> Storage
fromString rawStorage =
    rawStorage
        |> Json.Decode.decodeString decoder
        |> Result.withDefault { player = Nothing }


decoder : Json.Decode.Decoder Storage
decoder =
    Json.Decode.succeed Storage
        |> optional "player" storedPlayerDecoder Nothing


storedPlayerDecoder : Json.Decode.Decoder (Maybe PlayerIdentity)
storedPlayerDecoder =
    Json.Decode.field "identity" Core.Player.Identity.decoder
        |> Json.Decode.map Just
