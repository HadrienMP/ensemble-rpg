module Core.Player exposing (..)

import AssocList as Dict exposing (Dict)
import Core.Animals
import Core.Role exposing (Role)
import Core.RoleCard as RoleCard exposing (RoleCard)
import Random
import Core.XpProgress exposing (XpProgress)


type alias Player =
    { name : String
    , xp : Dict Role Int
    }


generator : Random.Generator Player
generator =
    Core.Animals.random |> Random.map (\animal -> Player animal Dict.empty)


unknown : Player
unknown =
    { name = "Unknown", xp = Dict.empty }


xpOf : RoleCard msg -> Player -> XpProgress
xpOf roleCard player =
    { current = Dict.get roleCard.role player.xp |> Maybe.withDefault 0
    , max = roleCard.xpToComplete
    , role = roleCard.role
    }


updateXp : Int -> Role -> Player -> Player
updateXp xp role player =
    { player | xp = Dict.insert role xp player.xp }


gainXp : RoleCard msg -> Player -> Player
gainXp roleCard player =
    let
        xp =
            RoleCard.incXp (xpOf roleCard player)
    in
    { player | xp = Dict.insert xp.role xp.current player.xp }


withName : String -> Player -> Player
withName name player =
    { player | name = name }


numberOfBadgesWon : Player -> Int
numberOfBadgesWon player =
    List.length <| badgesWon player


badgesWon : Player -> List (RoleCard msg)
badgesWon player =
    player.xp
        |> Dict.toList
        |> List.map (Tuple.mapFirst RoleCard.fromRole)
        |> List.filter (\( role, xp ) -> xp >= role.xpToComplete)
        |> List.map Tuple.first
