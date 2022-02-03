port module Js.Events exposing (..)

import Core.PlayerId as PlayerId exposing (..)
import Core.Role exposing (Role(..))
import Core.RoleCard
import Core.XpProgress
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Test.Html.Event exposing (Event)


port publishEvent : Json.Encode.Value -> Cmd msg


port receiveOne : (Json.Encode.Value -> msg) -> Sub msg


port receiveHistory : (List Json.Encode.Value -> msg) -> Sub msg


type EventDetails
    = NameChanged String
    | RoleCompleted Role


type alias Event =
    { playerId : PlayerId, details : EventDetails }


encode : Event -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ( "playerId", PlayerId.encode event.playerId )
        , ( "details", encodeDetails event.details )
        ]


encodeDetails : EventDetails -> Json.Encode.Value
encodeDetails eventDetails =
    case eventDetails of
        NameChanged name ->
            Json.Encode.object
                [ ( "event", Json.Encode.string "NamedChanged" )
                , ( "name", Json.Encode.string name )
                ]

        RoleCompleted role ->
            Json.Encode.object
                [ ( "event", Json.Encode.string "CompletedRole" )
                , ( "role", Core.RoleCard.fromRole role |> .id |> Json.Encode.string )
                ]


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.succeed Event
        |> required "playerId" PlayerId.decoder
        |> required "details" detailsDecoder


detailsDecoder : Decode.Decoder EventDetails
detailsDecoder =
    Decode.field "event" Decode.string
        |> Decode.andThen eventFromNameDecoder


eventFromNameDecoder : String -> Decode.Decoder EventDetails
eventFromNameDecoder eventName =
    case eventName of
        "NamedChanged" ->
            Decode.succeed NameChanged
                |> required "name" Decode.string

        "CompletedRole" ->
            Decode.succeed RoleCompleted
                |> required "role" roleDecoder

        _ ->
            Decode.fail <| "I don't know this event " ++ eventName


roleDecoder : Decode.Decoder Role
roleDecoder =
    Decode.string
        |> Decode.andThen
            (\roleName ->
                case roleName of
                    "Driver" ->
                        Decode.succeed Core.Role.Driver

                    "Navigator" ->
                        Decode.succeed Core.Role.Navigator

                    "Mobber" ->
                        Decode.succeed Core.Role.Mobber

                    "Researcher" ->
                        Decode.succeed Core.Role.Researcher

                    "Sponsor" ->
                        Decode.succeed Core.Role.Sponsor

                    "RearAdmiral" ->
                        Decode.succeed Core.Role.RearAdmiral

                    "Automationist" ->
                        Decode.succeed Core.Role.Automationist

                    "Nose" ->
                        Decode.succeed Core.Role.Nose

                    "Archivist" ->
                        Decode.succeed Core.Role.Archivist

                    "TrafficCop" ->
                        Decode.succeed Core.Role.TrafficCop

                    _ ->
                        Decode.fail <| "I don't know this role " ++ roleName
            )
