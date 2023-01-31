module Core.Player exposing (..)

import AssocList as Dict exposing (Dict)
import Core.Level exposing (..)
import Core.Player.Event exposing (EventData(..))
import Core.Player.Id as PlayerId exposing (..)
import Core.Player.Identity exposing (PlayerIdentity)
import Core.Player.Name
import Core.Role exposing (Role)
import Core.RoleCard as RoleCard exposing (RoleCard)
import Core.XpProgress exposing (XpProgress)
import Random
import Random.Char



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


generator : Random.Generator Player
generator =
    playerIdentityGenerator
    |> Random.map fromIdentity


playerIdentityGenerator : Random.Generator PlayerIdentity
playerIdentityGenerator =
    Random.map3
        PlayerIdentity
        PlayerId.generator
        Random.Char.emoticon
        Core.Player.Name.generator



-- Rest


reset : Player -> Player
reset player =
    { player | completedRoles = Dict.empty, xp = Dict.empty }


evolve : Core.Player.Event.EventData -> Player -> Player
evolve data player =
    case data of
        ChangedIdentity identity ->
            { player | identity = Debug.log "changed identity" identity }

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
