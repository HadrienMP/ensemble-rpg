module Core.Player exposing (..)

import AssocList as Dict exposing (Dict)
import Core.Role exposing (Role)
import Core.RoleCard as RoleCard exposing (RoleCard)
import Core.Animals
import Random


type alias Player =
    { name : String
    , xp : Dict Role Int
    }

generator : Random.Generator Player
generator = Core.Animals.random |> Random.map (\animal -> Player animal Dict.empty)

unknown : Player
unknown =
    { name = "Unknown", xp = Dict.empty }


xpOf : Role -> Player -> Int
xpOf role player =
    Dict.get role player.xp
        |> Maybe.withDefault 0


updateXp : Int -> Role -> Player -> Player
updateXp xp role player =
    { player | xp = Dict.insert role xp player.xp }


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
