module Pages.Team exposing (page)

import Color.Dracula
import Core.OtherPlayer exposing (OtherPlayer)
import Core.RoleCard as RoleCard exposing (DisplayMode(..))
import Element exposing (..)
import Element.Font as Font
import Page exposing (Page)
import Request exposing (Request)
import Shared
import UI.Theme exposing (CardSize(..), container)
import View exposing (View)


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


displayPlayer : OtherPlayer -> Element msg
displayPlayer player =
    row [ spacing 10, width fill ]
        [ UI.Theme.card []
            { icon = el [ Font.size 30, centerX ] <| text <| String.fromChar player.icon
            , color = Color.Dracula.green
            , size = Small
            , main = text player.name
            , sub = text <| (String.fromInt <| List.length player.completedRoles) ++ " badges"
            }
        , wrappedRow [ spacing 10, width fill ]
            (player.completedRoles
                |> List.map RoleCard.fromRole
                |> List.map (RoleCard.view Badge)
            )
        ]
