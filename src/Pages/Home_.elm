module Pages.Home_ exposing (page)

import Color.Dracula
import Core.AccessibleRoles
import Core.Player as Player
import Core.RoleCard as RoleCard exposing (DisplayMode(..), RoleCard)
import Core.XpProgress exposing (XpProgress)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Route as Route
import Page exposing (Page)
import Request exposing (Request)
import Shared
import UI.Theme exposing (darken)
import View exposing (View)


page : Shared.Model -> Request -> Page
page shared _ =
    Page.protected.static <|
        \_ ->
            { view = view shared
            }


view : Shared.Model -> View msg
view shared =
    { title = "Homepage"
    , body =
        UI.Theme.container
            { profile = shared.profile
            , currentRoute = Just Route.Home_
            }
            []
        <|
            roleCardsView shared
    }


roleCardsView : Shared.Model -> Element msg
roleCardsView shared =
    let
        accessibleRoles =
            Core.AccessibleRoles.list shared.player
    in
    column [ width fill, spacing 20 ]
        [ accessibleRoles.roles
            |> List.map (roleView shared)
            |> wrappedRow [ spacing 10, width fill ]
        , accessibleRoles.nextLevelRule
            |> Maybe.map
                (el
                    [ Font.color <| darken 2 <| Color.Dracula.white
                    , Font.size 16
                    ]
                    << text
                )
            |> Maybe.withDefault Element.none
        ]


roleView : Shared.Model -> RoleCard msg -> Element msg
roleView shared role =
    Element.link [ width <| maximum 122 <| fill ]
        { url = Route.Role__Id_ { id = role.id } |> Route.toHref
        , label =
            el
                [ inFront <| displayXp <| Player.progressOf role.role shared.player
                , width fill
                ]
                (RoleCard.view Card role)
        }


displayXp : XpProgress -> Element msg
displayXp { current, max } =
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
