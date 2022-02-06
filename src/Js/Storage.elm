port module Js.Storage exposing (..)

import Core.Player exposing (PlayerIdentity)
import Core.PlayerId exposing (PlayerId)
import Json.Encode


port savePlayerIdentity : Json.Encode.Value -> Cmd msg


save : PlayerId -> PlayerIdentity -> Cmd msg
save id identity =
    Json.Encode.object
        [ ( "id", Core.PlayerId.encode id )
        , ( "identity", Core.Player.encodeIdentity identity )
        ]
        |> savePlayerIdentity
