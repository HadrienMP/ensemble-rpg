module Core.Player exposing (..)

import AssocList as Dict exposing (Dict)
import Core.Animals
import Core.Role exposing (Role)
import Core.RoleCard as RoleCard exposing (RoleCard)
import Core.XpProgress exposing (XpProgress)
import Random
import Core.Level exposing (..)
import Core.XpProgress exposing (completed)


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


progressOf : Role -> Player -> XpProgress
progressOf role player =
    let
        roleCard =
            RoleCard.fromRole role
    in
    { current = Dict.get roleCard.role player.xp |> Maybe.withDefault 0
    , max = roleCard.xpToComplete
    , role = roleCard.role
    }


updateXp : Int -> Role -> Player -> Player
updateXp xp role player =
    { player | xp = Dict.insert role xp player.xp }


gainXp : Role -> Player -> Player
gainXp role player =
    let
        xp =
            RoleCard.incXp (progressOf role player)
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
    badgesWon player
        |> List.map .level
        |> List.filter (\l -> l == level)
        |> List.length



accessibleRoles : Player -> List (RoleCard msg)
accessibleRoles player =
    let 
        levels = accessibleLevels player
    in
    RoleCard.all
    |> List.filter (\card -> List.member card.level levels)
    |> List.filter (\card -> progressOf card.role player |> not << completed)