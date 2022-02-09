module Player__Spec exposing (..)

import Core.Level exposing (Level(..))
import Core.Player exposing (Event(..), Player)
import Core.Player.Id exposing (PlayerId(..))
import Core.Role
import Expect
import Test exposing (..)
import Core.AccessibleRoles exposing (accessibleLevels)


suite : Test
suite =
    describe "Player"
        [ describe "Accessible Roles"
            [ test "Only level 1 roles are accessible at first" <|
                \_ ->
                    joinGame
                        |> accessibleLevels
                        |> Expect.equal
                            { levels = [ Level1 ]
                            , nextLevelRule = Just "Complete one role to unlock level 2 roles"
                            }
            , describe "To access level 2 roles a player needs to complete at least one level 1 role"
                [ test "Scenario: A single role" <|
                    \_ ->
                        joinGame
                            |> completeRole Core.Role.Mobber
                            |> accessibleLevels
                            |> Expect.equal
                                { levels = [ Level1, Level2 ]
                                , nextLevelRule = Just "Complete two level 2 roles to unlock level 3 roles"
                                }
                , test "Scenario: two roles" <|
                    \_ ->
                        joinGame
                            |> completeRole Core.Role.Mobber
                            |> completeRole Core.Role.Navigator
                            |> accessibleLevels
                            |> Expect.equal
                                { levels = [ Level1, Level2 ]
                                , nextLevelRule = Just "Complete two level 2 roles to unlock level 3 roles"
                                }
                ]
            , describe "To access level 3 roles a player needs to complete at least two level 2 roles"
                [ test "Scenario: Two roles" <|
                    \_ ->
                        joinGame
                            |> completeRole Core.Role.Mobber
                            |> completeRole Core.Role.Sponsor
                            |> completeRole Core.Role.Researcher
                            |> accessibleLevels
                            |> Expect.equal
                                { levels = [ Level1, Level2, Level3 ]
                                , nextLevelRule = Just "Complete two level 3 roles to unlock level 4 roles"
                                }
                , test "Scenario: Three roles" <|
                    \_ ->
                        joinGame
                            |> completeRole Core.Role.Mobber
                            |> completeRole Core.Role.Sponsor
                            |> completeRole Core.Role.Researcher
                            |> completeRole Core.Role.RearAdmiral
                            |> accessibleLevels
                            |> Expect.equal
                                { levels = [ Level1, Level2, Level3 ]
                                , nextLevelRule = Just "Complete two level 3 roles to unlock level 4 roles"
                                }
                , test "Scenario: a single role on level 2 but all of the level 1" <|
                    \_ ->
                        joinGame
                            |> completeRole Core.Role.Mobber
                            |> completeRole Core.Role.Navigator
                            |> completeRole Core.Role.Driver
                            |> completeRole Core.Role.Sponsor
                            |> accessibleLevels
                            |> Expect.equal
                                { levels = [ Level1, Level2 ]
                                , nextLevelRule = Just "Complete two level 2 roles to unlock level 3 roles"
                                }
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
                            |> Expect.equal
                                { levels = [ Level1, Level2, Level3, Level4 ]
                                , nextLevelRule = Nothing
                                }
                ]
            ]
        ]



-- Test utils


completeRole : Core.Role.Role -> Player -> Player
completeRole role player =
    List.repeat 4 (DisplayedBehaviour role)
        |> evolveMany player


evolveMany : Player -> List Event -> Player
evolveMany model events =
    case events of
        [] ->
            model

        head :: tail ->
            evolveMany (Core.Player.evolve head model) tail


joinGame : Player
joinGame =
    Core.Player.unknown
        |> Core.Player.evolve (ChangedIdentity { id = PlayerId "id", name = "Jane", icon = 'J' })
