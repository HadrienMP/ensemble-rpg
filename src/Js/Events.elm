port module Js.Events exposing (..)

import Core.Player.Event
import Core.Player.Id exposing (..)
import Core.Role exposing (Role(..))
import Core.Room exposing (Room)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Test.Html.Event exposing (Event)



port join : { room : String } -> Cmd msg


port publishEvent : Json.Encode.Value -> Cmd msg


port receiveOne : (Json.Encode.Value -> msg) -> Sub msg


port receiveHistory : (Json.Encode.Value -> msg) -> Sub msg


listenToOne : (Result Decode.Error Super -> msg) -> Sub msg
listenToOne msgF =
    receiveOne (\value -> Decode.decodeValue superDecode value |> msgF)


listenToHistory : (Result Decode.Error (List Super) -> msg) -> Sub msg
listenToHistory msgF =
    receiveHistory (\value -> Decode.decodeValue (Decode.list superDecode) value |> msgF)


publish : Super -> Cmd msg
publish event =
    encode event |> publishEvent


type alias Super =
    { room : Room, content : Event }


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


encode : Super -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ( "room", Json.Encode.string <| Core.Room.print event.room )
        , ( "content"
          , Json.Encode.object
                [ ( "type", Json.Encode.string <| eventType event.content )
                , ( "data", encodeEventData event.content )
                ]
          )
        ]


encodeEventData : Event -> Json.Encode.Value
encodeEventData event =
    case event of
        PlayerEvent playerEvent ->
            Core.Player.Event.encode playerEvent

        Reset ->
            Json.Encode.null


superDecode : Decode.Decoder Super
superDecode =
    Decode.succeed Super
        |> required "room" (Decode.string |> Decode.map Core.Room.fromString)
        |> required "content" eventDecoder


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
