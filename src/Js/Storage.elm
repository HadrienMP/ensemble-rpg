port module Js.Storage exposing (..)

import Core.Player exposing (PlayerIdentity)
import Core.PlayerId exposing (PlayerId)
import Json.Decode
import Json.Decode.Pipeline exposing (optional)
import Json.Encode
import Json.Decode.Pipeline exposing (required)


port saveInStorage : Json.Encode.Value -> Cmd msg


type alias StoragePlayer =
    { id : PlayerId
    , identity : PlayerIdentity
    }


type alias Storage =
    { player : Maybe StoragePlayer
    }


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
                            [ ( "id", Core.PlayerId.encode player.id )
                            , ( "identity", Core.Player.encodeIdentity player.identity )
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


storedPlayerDecoder : Json.Decode.Decoder (Maybe StoragePlayer)
storedPlayerDecoder =
    Json.Decode.succeed StoragePlayer
        |> required "id" Core.PlayerId.decoder
        |> required "identity" Core.Player.identityDecoder
        |> Json.Decode.map Just
