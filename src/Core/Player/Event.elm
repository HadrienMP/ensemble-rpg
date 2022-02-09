module Core.Player.Event exposing (..)

import Core.Player.Identity exposing (PlayerIdentity)
import Core.Role exposing (Role)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Core.RoleCard


type Event
    = ChangedIdentity PlayerIdentity
    | DisplayedBehaviour Role


encode : Event -> Json.Encode.Value
encode event =
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


decoder : Json.Decode.Decoder Event
decoder =
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
