module Pages.Team exposing (page)

import Color.Dracula
import Core.Player as Player exposing (Player)
import Core.RoleCard as RoleCard exposing (DisplayMode(..))
import Element exposing (column, row, spacing, spacingXY, text, wrappedRow)
import Page exposing (Page)
import Request exposing (Request)
import Shared
import UI.Theme exposing (CardSize(..), container)
import View exposing (View)
import Element exposing (..)
import Element.Font as Font


page : Shared.Model -> Request -> Page
page shared _ =
    Page.static { view = view shared.player }




-- VIEW


view : Player -> View msg
view player =
    { title = "Team"
    , body =
        container [] <|
            column [ spacingXY 0 20 ]
                [ row [ spacing 10 ]
                    [ UI.Theme.card []
                        { icon = el [Font.size 30, centerX] <| text <| String.fromChar player.icon
                        , color = Color.Dracula.green
                        , size = Small
                        , main = text player.name
                        , sub = text <| (String.fromInt <| Player.numberOfBadgesWon player) ++ " badges"
                        }
                    , wrappedRow [ spacing 10, width fill ]
                        (Player.badgesWon player
                            |> List.map (RoleCard.view Badge)
                        )
                    ]
                ]
    }
