module Player__Spec exposing (..)

import Core.Level exposing (Level(..))
import Core.Player exposing (Player, accessibleLevels)
import Core.Role
import AssocList as Dict
import Expect
import Test exposing (..)
import Core.PlayerId


suite : Test
suite =
    describe "Accessible Roles"
        [ test "Only level 1 roles are accessible at first" <|
            \_ ->
                joinGame
                    |> accessibleLevels
                    |> Expect.equal [ Level1 ]
        , describe "To access level 2 roles a player needs to complete at least one level 1 role"
            [ test "Scenario: A single role" <|
                \_ ->
                    joinGame
                        |> completeRole Core.Role.Mobber
                        |> accessibleLevels
                        |> Expect.equal [ Level1, Level2 ]
            , test "Scenario: two roles" <|
                \_ ->
                    joinGame
                        |> completeRole Core.Role.Mobber
                        |> completeRole Core.Role.Navigator
                        |> accessibleLevels
                        |> Expect.equal [ Level1, Level2 ]
            ]
        , describe "To access level 3 roles a player needs to complete at least two level 2 roles"
            [ test "Scenario: Two roles" <|
                \_ ->
                    joinGame
                        |> completeRole Core.Role.Mobber
                        |> completeRole Core.Role.Sponsor
                        |> completeRole Core.Role.Researcher
                        |> accessibleLevels
                        |> Expect.equal [ Level1, Level2, Level3 ]
            , test "Scenario: Three roles" <|
                \_ ->
                    joinGame
                        |> completeRole Core.Role.Mobber
                        |> completeRole Core.Role.Sponsor
                        |> completeRole Core.Role.Researcher
                        |> completeRole Core.Role.RearAdmiral
                        |> accessibleLevels
                        |> Expect.equal [ Level1, Level2, Level3 ]
            ]
        , describe "To access level 4 roles a player needs to complete at least two level 2 roles"
            [ test "Scenario: Two roles" <|
                \_ ->
                    joinGame
                        |> completeRole Core.Role.Mobber
                        |> completeRole Core.Role.Sponsor
                        |> completeRole Core.Role.Researcher
                        |> completeRole Core.Role.Archivist
                        |> completeRole Core.Role.Automationist
                        |> accessibleLevels
                        |> Expect.equal [ Level1, Level2, Level3, Level4 ]
            ]
        ]



-- Test utils


completeRole : Core.Role.Role -> Player -> Player
completeRole role =
    Core.Player.displayedBeheviour role
        >> Core.Player.displayedBeheviour role
        >> Core.Player.displayedBeheviour role
        >> Core.Player.displayedBeheviour role


joinGame : Player
joinGame =
    { id = Core.PlayerId.empty
    , name = "Jane"
    , icon = 'c'
    , xp = Dict.empty
    }
