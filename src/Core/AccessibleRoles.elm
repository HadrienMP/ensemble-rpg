module Core.AccessibleRoles exposing (..)

import Core.Level exposing (Level(..))
import Core.Player exposing (Player)
import Core.RoleCard exposing (RoleCard)
import Core.XpProgress


list : Player -> { roles : List (RoleCard msg), nextLevelRule : Maybe String }
list player =
    let
        levels : { levels : List Level, nextLevelRule : Maybe String }
        levels =
            accessibleLevels player
    in
    { roles =
        Core.RoleCard.all
            |> List.filter (\card -> List.member card.level levels.levels)
            |> List.filter (\card -> Core.Player.progressOf card.role player |> not << Core.XpProgress.completed)
    , nextLevelRule = levels.nextLevelRule
    }


accessibleLevels : Player -> { levels : List Level, nextLevelRule : Maybe String }
accessibleLevels player =
    let
        accessible =
            Core.Level.all
                |> List.filter (isLevelAccessibleTo player)
    in
    { levels = accessible
    , nextLevelRule = nextLevelRule accessible
    }


nextLevelRule : List Level -> Maybe String
nextLevelRule levels =
    case levels |> List.sortBy Core.Level.toString |> List.reverse |> List.head of
        Just Level3 ->
            Just "Complete two level 3 roles to unlock level 4 roles"

        Just Level2 ->
            Just "Complete two level 2 roles to unlock level 3 roles"

        Just Level1 ->
            Just "Complete one role to unlock level 2 roles"

        _ ->
            Nothing


isLevelAccessibleTo : Player -> Level -> Bool
isLevelAccessibleTo player level =
    case level of
        Level1 ->
            True

        Level2 ->
            countRolesCompleted player Level1 >= 1

        _ ->
            Core.Level.previous level
                |> Maybe.map (countRolesCompleted player)
                |> Maybe.map (\count -> count >= 2)
                |> Maybe.withDefault False


countRolesCompleted : Player -> Level -> Int
countRolesCompleted player level =
    Core.Player.completedRoleCards player
        |> List.map .level
        |> List.filter (\l -> l == level)
        |> List.length
