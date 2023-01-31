module Pages.Team exposing (page)

import AssocList as Dict
import Color.Dracula
import Core.Player exposing (Player)
import Core.RoleCard as RoleCard exposing (DisplayMode(..))
import Element exposing (..)
import Element.Font as Font
import Gen.Route
import Page exposing (Page)
import Request exposing (Request)
import Shared
import UI.Theme exposing (CardSize(..), container, h2)
import View exposing (View)


page : Shared.Model -> Request -> Page
page shared _ =
    Page.protected.static <| \_ -> { view = view shared }



-- VIEW


view : Shared.Model -> View msg
view model =
    { title = "Team"
    , body =
        container { profile = model.profile, currentRoute = Just Gen.Route.Team } [] <|
            column [ spacingXY 0 10 ]
                ((h2 [] <|
                    row [ spacingXY 10 0 ]
                        [ text "Team Score:"
                        , el [ Font.bold ] <| text <| (String.fromInt <| Shared.score model) ++ " badges"
                        ]
                 )
                    :: (Shared.allPlayers model |> List.map displayPlayer)
                )
    }


displayPlayer : Player -> Element msg
displayPlayer player =
    row [ spacing 10, width fill ]
        [ UI.Theme.card []
            { icon = el [ Font.size 30, centerX ] <| text <| String.fromChar player.identity.icon
            , color = Color.Dracula.green
            , size = Small
            , main = text player.identity.name
            , sub = text <| (String.fromInt <| Dict.size player.completedRoles) ++ " badges"
            }
        , wrappedRow [ spacing 10, width fill ]
            (Core.Player.completedRoleCards player
                |> List.map (RoleCard.view Badge)
            )
        ]
