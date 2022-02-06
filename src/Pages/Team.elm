module Pages.Team exposing (page)

import Color.Dracula
import Core.RoleCard as RoleCard exposing (DisplayMode(..))
import Core.Player exposing (Player)
import Element exposing (..)
import Element.Font as Font
import Page exposing (Page)
import Request exposing (Request)
import Shared
import UI.Theme exposing (CardSize(..), container)
import View exposing (View)
import AssocList as Dict


page : Shared.Model -> Request -> Page
page shared _ =
    Page.static { view = view shared }



-- VIEW


view : Shared.Model -> View msg
view model =
    { title = "Team"
    , body =
        container [] <|
            column [ spacingXY 0 10 ]
                (Shared.allPlayers model |> List.map displayPlayer)
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
