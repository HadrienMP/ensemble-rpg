module Pages.Home_ exposing (page, view)

import Color.Dracula
import Core.Level exposing (Level(..))
import Core.Player as Player
import Core.Role.Card.RoleCard as RoleCard exposing (RoleCard, displayXpSlots)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Gen.Route as Route
import Page exposing (Page)
import Request exposing (Request)
import Shared
import UI.Icons
import UI.Theme
import View exposing (View)
import Core.Player exposing (xpOf)


page : Shared.Model -> Request -> Page
page shared req =
    Page.static
        { view = view shared
        }


view : Shared.Model -> View msg
view shared =
    { title = "Homepage"
    , body =
        UI.Theme.container <| roleCardsView shared
    }


roleCardsView : Shared.Model -> Element msg
roleCardsView shared =
    RoleCard.all
        |> List.filter (\card -> xpOf card.role shared.player < card.xpToComplete)
        |> List.map (roleView shared)
        |> wrappedRow [ spacing 10 ]


roleView : Shared.Model -> RoleCard msg -> Element msg
roleView shared role =
    Element.link [ width fill ]
        { url = Route.Role__Id_ { id = role.id } |> Route.toHref
        , label =
            el
                [ inFront <| displayXp (Player.xpOf role.role shared.player) role.xpToComplete
                , width fill
                ]
            <|
                RoleCard.cardView role
        }


displayXp : Int -> RoleCard.XpToComplete -> Element msg
displayXp current max =
    max
        |> List.range 1
        |> List.map
            (\xp ->
                if xp <= current then
                    [ Background.color Color.Dracula.green ]

                else
                    []
            )
        |> List.map (\attr -> el (attr ++ [ width fill, height (px 4) ]) none)
        |> row [ width fill, height (px 5), alignBottom, paddingXY 2 5, Border.rounded 50 ]
