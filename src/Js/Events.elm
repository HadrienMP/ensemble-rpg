port module Js.Events exposing (..)

import Core.PlayerId as PlayerId exposing (..)
import Core.Role exposing (Role(..))
import Core.Player
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Test.Html.Event exposing (Event)


port ready : () -> Cmd msg

port publishEvent : Json.Encode.Value -> Cmd msg


port receiveOne : (Json.Encode.Value -> msg) -> Sub msg


port receiveHistory : (Json.Encode.Value -> msg) -> Sub msg

listenToOne : (Result Decode.Error Event -> msg) -> Sub msg
listenToOne msgF =
    receiveOne (\value -> Decode.decodeValue eventDecoder value |> msgF)

listenToHistory : (Result Decode.Error (List Event) -> msg) -> Sub msg
listenToHistory msgF =
    receiveHistory (\value -> Decode.decodeValue (Decode.list eventDecoder) value |> msgF)


publish : Event -> Cmd msg
publish event =
    encode event |> publishEvent


type alias Event =
    { playerId : PlayerId, playerEvent : Core.Player.Event }


encode : Event -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ( "playerId", PlayerId.encode event.playerId )
        , ( "event", Core.Player.encodeEvent event.playerEvent )
        ]


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.succeed Event
        |> required "playerId" PlayerId.decoder
        |> required "event" Core.Player.eventDecoder