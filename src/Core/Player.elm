module Core.Player exposing (..)

import AssocList as Dict exposing (Dict)
import Core.Level exposing (..)
import Core.Player.Id as PlayerId exposing (..)
import Core.Player.Name
import Core.Role exposing (Role)
import Core.RoleCard as RoleCard exposing (RoleCard)
import Core.XpProgress exposing (XpProgress)
import Js.DecoderExtra exposing (firstCharDecoder)
import Json.Decode
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode
import Random
import Random.Char
import Core.Player.Identity exposing (PlayerIdentity)



-- Model


type alias Player =
    { identity : PlayerIdentity
    , completedRoles : Dict Role ()
    , xp : Dict Role XpProgress
    }



-- Factory


fromIdentity : PlayerIdentity -> Player
fromIdentity identity =
    { identity = identity
    , completedRoles = Dict.empty
    , xp = Dict.empty
    }


unknown : Player
unknown =
    Player unknownIdentity Dict.empty Dict.empty


unknownIdentity : PlayerIdentity
unknownIdentity =
    { id = PlayerId "???", icon = 'U', name = "Unknown" }



-- Generator


generator : Random.Generator ( Player, Event )
generator =
    Random.map (Tuple.mapFirst fromIdentity) playerIdentityGenerator


playerIdentityGenerator : Random.Generator ( PlayerIdentity, Event )
playerIdentityGenerator =
    Random.map3
        PlayerIdentity
        PlayerId.generator
        Random.Char.emoticon
        Core.Player.Name.generator
        |> Random.map (\identity -> ( identity, ChangedIdentity identity ))



-- Event


type Event
    = ChangedIdentity PlayerIdentity
    | DisplayedBehaviour Role


encodeEvent : Event -> Json.Encode.Value
encodeEvent event =
    case event of
        ChangedIdentity identity ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "ChangedIdentity" )
                , ( "data"
                  , Json.Encode.object
                        [ ( "id", PlayerId.encode identity.id )
                        , ( "icon", Json.Encode.string <| String.fromChar identity.icon )
                        , ( "name", Json.Encode.string identity.name )
                        ]
                  )
                ]

        DisplayedBehaviour role ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "DisplayedBehaviour" )
                , ( "data", RoleCard.encode role )
                ]


eventDecoder : Json.Decode.Decoder Event
eventDecoder =
    Json.Decode.field "type" Json.Decode.string
        |> Json.Decode.andThen
            (\eventName ->
                case eventName of
                    "ChangedIdentity" ->
                        Json.Decode.succeed PlayerIdentity
                            |> requiredAt [ "data", "id" ] PlayerId.decoder
                            |> requiredAt [ "data", "icon" ] firstCharDecoder
                            |> requiredAt [ "data", "name" ] Json.Decode.string
                            |> Json.Decode.map ChangedIdentity

                    "DisplayedBehaviour" ->
                        Json.Decode.succeed DisplayedBehaviour
                            |> required "data" RoleCard.decoder

                    _ ->
                        Json.Decode.fail <| "This is an unknown event: " ++ eventName
            )



-- Rest


reset : Player -> Player
reset player =
    { player | completedRoles = Dict.empty, xp = Dict.empty }


evolve : Event -> Player -> Player
evolve event player =
    case event of
        ChangedIdentity identity ->
            { player | identity = identity }

        DisplayedBehaviour role ->
            let
                progress =
                    progressOf role player |> Core.XpProgress.increment
            in
            { player
                | xp = Dict.insert role progress player.xp
                , completedRoles =
                    if Core.XpProgress.completed progress then
                        Dict.insert role () player.completedRoles

                    else
                        player.completedRoles
            }



-- Functions


progressOf : Role -> Player -> XpProgress
progressOf target player =
    player.xp
        |> Dict.get target
        |> Maybe.withDefault (Core.XpProgress.empty target)


numberOfBadgesWon : Player -> Int
numberOfBadgesWon player =
    List.length <| completedRoleCards player


completedRoleCards : Player -> List (RoleCard msg)
completedRoleCards player =
    player.completedRoles |> Dict.keys |> List.map RoleCard.fromRole

