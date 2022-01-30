module Pages.Home_ exposing (view)

import AssocList as Dict
import Browser.Dom exposing (blur)
import Color.Dracula
import Domain.Level exposing (Level(..), toString)
import Domain.RoleCard exposing (RoleCard)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Route as Route
import Html.Attributes
import String exposing (toInt)
import Svg.Attributes exposing (fontWeight)
import Theme
import View exposing (View)


view : View msg
view =
    { title = "Homepage"
    , body =
        Theme.container
            [ el Theme.h1 <| text "Roles"
            , roleCardsView
            ]
    }


roleCardsView : Element msg
roleCardsView =
    Domain.RoleCard.all
        |> List.map roleView
        |> wrappedRow [ spacing 10 ]


roleView : RoleCard msg -> Element msg
roleView role =
    let
        cardWidth =
            115

        cardHeight : Int
        cardHeight =
            toFloat cardWidth
                |> (*) 1.4
                |> round
    in
    Element.link
        [ Border.rounded 5
        , Border.solid
        , Border.width 2
        , Border.color Color.Dracula.white
        , Background.color <| Theme.darken 4 <| colorOf role.level
        , height <| px cardHeight
        , width <| minimum cardWidth fill
        ]
        { url = Route.Role__Id_ { id = role.id } |> Route.toHref
        , label =
            column [ spacingXY 0 5, width fill ]
                [ el [ width <| px 100, centerX, paddingXY 15 0 ] role.icon
                , el
                    [ centerX
                    , Font.bold
                    , Font.size 14  
                    , paddingXY 0 5
                    , Font.shadow { offset = ( 2, 2 ), blur = 2, color = Color.Dracula.gray }

                    -- , htmlAttribute <| Html.Attributes.style "-webkit-text-stroke" "1px black"
                    ]
                    (text role.label)
                , el
                    [ centerX
                    , Font.size 12
                    , Font.shadow { offset = ( 1, 1 ), blur = 2, color = Color.Dracula.gray }
                    ]
                    (text <| "Level " ++ Domain.Level.toString role.level)
                ]
        }

colorOf : Level -> Color
colorOf level =
    case level of
        Level1 ->
            Color.Dracula.purple

        Level2 ->
            Color.Dracula.pink

        Level3 ->
            Color.Dracula.orange

        Level4 ->
            Color.Dracula.red
