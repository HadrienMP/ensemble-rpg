module Core.Player exposing (..)

import AssocList as Dict exposing (Dict)
import Core.Level exposing (..)
import Core.PlayerId as PlayerId exposing (..)
import Core.PlayerName
import Core.Role exposing (Role)
import Core.RoleCard as RoleCard exposing (RoleCard)
import Core.XpProgress exposing (XpProgress, completed)
import Js.DecoderExtra exposing (firstCharDecoder)
import Json.Decode
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode
import Random
import Random.Char


type alias PlayerIdentity =
    { icon : Char
    , name : String
    }


type Event
    = ChangedIdentity PlayerIdentity
    | DisplayedBehaviour Role


type alias Player =
    { id : PlayerId
    , identity : PlayerIdentity
    , completedRoles : List Role
    , xp : Dict Role XpProgress
    }


unknown : Player
unknown =
    Player PlayerId.empty unknownIdentity [] Dict.empty


unknownIdentity : PlayerIdentity
unknownIdentity =
    { icon = 'U', name = "Unknown" }


evolve : Event -> Player -> Player
evolve event player =
    case event of
        ChangedIdentity identity ->
            { player | identity = identity }

        DisplayedBehaviour role ->
            let
                progress =
                    Dict.get role player.xp |> Maybe.withDefault (Core.XpProgress.empty role) |> Core.XpProgress.increment
            in
            { player
                | xp = Dict.insert role progress player.xp
                , completedRoles =
                    if Core.XpProgress.completed progress then
                        role :: player.completedRoles

                    else
                        player.completedRoles
            }


encodeIdentity : PlayerIdentity -> Json.Encode.Value
encodeIdentity identity =
    Json.Encode.object
        [ ( "icon", Json.Encode.string <| String.fromChar identity.icon )
        , ( "name", Json.Encode.string identity.name )
        ]

identityDecoder : Json.Decode.Decoder PlayerIdentity
identityDecoder =
    Json.Decode.succeed PlayerIdentity
    |> required "icon" firstCharDecoder
    |> required "name" Json.Decode.string


encodeEvent : Event -> Json.Encode.Value
encodeEvent event =
    case event of
        ChangedIdentity identity ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "ChangedIdentity" )
                , ( "data"
                  , Json.Encode.object
                        [ ( "icon", Json.Encode.string <| String.fromChar identity.icon )
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
                            |> requiredAt [ "data", "icon" ] firstCharDecoder
                            |> requiredAt [ "data", "name" ] Json.Decode.string
                            |> Json.Decode.map ChangedIdentity

                    "DisplayedBehaviour" ->
                        Json.Decode.succeed DisplayedBehaviour
                            |> required "data" RoleCard.decoder

                    _ ->
                        Json.Decode.fail <| "This is an unknown event: " ++ eventName
            )



--


type alias PlayerWithIdentityEvent =
    { player : Player, event : Event }


generator : Random.Generator PlayerWithIdentityEvent
generator =
    Random.map2
        (\id identityResult ->
            { player =
                { id = id
                , identity = identityResult.identity
                , completedRoles = []
                , xp = Dict.empty
                }
            , event = identityResult.event
            }
        )
        PlayerId.generator
        playerIdentityGenerator


playerIdentityGenerator : Random.Generator { identity : PlayerIdentity, event : Event }
playerIdentityGenerator =
    Random.map2 PlayerIdentity Random.Char.emoticon Core.PlayerName.generator
        |> Random.map (\identity -> { identity = identity, event = ChangedIdentity identity })


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
    player.completedRoles |> List.map RoleCard.fromRole


accessibleLevels : Player -> List Level
accessibleLevels player =
    all
        |> List.filter (isLevelAccessibleTo player)


isLevelAccessibleTo : Player -> Level -> Bool
isLevelAccessibleTo player level =
    case level of
        Level1 ->
            True

        Level2 ->
            countRolesCompleted player Level1 >= 1

        _ ->
            previous level
                |> Maybe.map (countRolesCompleted player)
                |> Maybe.map (\count -> count >= 2)
                |> Maybe.withDefault False


countRolesCompleted : Player -> Level -> Int
countRolesCompleted player level =
    completedRoleCards player
        |> List.map .level
        |> List.filter (\l -> l == level)
        |> List.length


accessibleRoles : Player -> List (RoleCard msg)
accessibleRoles player =
    let
        levels =
            accessibleLevels player
    in
    RoleCard.all
        |> List.filter (\card -> List.member card.level levels)
        |> List.filter (\card -> progressOf card.role player |> not << completed)
