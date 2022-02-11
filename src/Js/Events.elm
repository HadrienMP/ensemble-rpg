port module Js.Events exposing (..)

import Core.Player.Event
import Core.Player.Id as PlayerId exposing (..)
import Core.Role exposing (Role(..))
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


type Event
    = PlayerEvent Core.Player.Event.Event
    | Reset


eventType : Event -> String
eventType event =
    case event of
        PlayerEvent _ ->
            "Player"

        Reset ->
            "Reset"


encode : Event -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ( "type", Json.Encode.string <| eventType event )
        , ( "data", encodeEventData event )
        ]


encodeEventData : Event -> Json.Encode.Value
encodeEventData event =
    case event of
        PlayerEvent playerEvent ->
            Core.Player.Event.encode playerEvent

        Reset ->
            Json.Encode.null


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "Player" ->
                        Decode.succeed PlayerEvent
                            |> required "data" Core.Player.Event.decoder

                    "Reset" ->
                        Decode.succeed Reset

                    _ ->
                        Decode.fail <| "I don't know this js event type: " ++ t
            )
