module Core.Player.Event exposing (..)

import Core.Player.Id exposing (PlayerId)
import Core.Player.Identity exposing (PlayerIdentity)
import Core.Role exposing (Role)
import Core.RoleCard
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Uuid exposing (Uuid)


type alias Event =
    { id : Uuid
    , playerId : PlayerId
    , data : EventData
    }


toEvent : Uuid -> PlayerId -> EventData -> Event
toEvent uuid playerId data =
    { id = uuid, playerId = playerId, data = data }


type EventData
    = ChangedIdentity PlayerIdentity
    | DisplayedBehaviour Role


encode : Event -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ( "id", Uuid.encode event.id )
        , ( "playerId", Core.Player.Id.encode event.playerId )
        , ( "data", encodeData event.data )
        ]


decoder : Json.Decode.Decoder Event
decoder =
    Json.Decode.succeed Event
        |> required "id" Uuid.decoder
        |> required "playerId" Core.Player.Id.decoder
        |> required "data" dataDecoder


encodeData : EventData -> Json.Encode.Value
encodeData event =
    case event of
        ChangedIdentity identity ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "ChangedIdentity" )
                , ( "data"
                  , Core.Player.Identity.encode identity
                  )
                ]

        DisplayedBehaviour role ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "DisplayedBehaviour" )
                , ( "data", Core.RoleCard.encode role )
                ]


dataDecoder : Json.Decode.Decoder EventData
dataDecoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\eventName ->
                case eventName of
                    "ChangedIdentity" ->
                        Json.Decode.succeed ChangedIdentity
                            |> required "data" Core.Player.Identity.decoder

                    "DisplayedBehaviour" ->
                        Json.Decode.succeed DisplayedBehaviour
                            |> required "data" Core.RoleCard.decoder

                    _ ->
                        Json.Decode.fail <| "This is an unknown event: " ++ eventName
            )
