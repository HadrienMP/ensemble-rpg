module Core.XpProgress exposing (..)

import Color.Dracula
import Core.Role exposing (Role)
import Core.RoleCard
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border


type alias XpProgress =
    { current : Int
    , max : Int
    , role : Role
    }


empty : Role -> XpProgress
empty role =
    { current = 0
    , role = role
    , max = Core.RoleCard.fromRole role |> .xpToComplete
    }


increment : XpProgress -> XpProgress
increment xp =
    { xp | current = min xp.max <| xp.current + 1 }


displayXpSlots : XpProgress -> Element msg
displayXpSlots progress =
    el
        [ width fill
        , Border.solid
        , Border.width 1
        , Border.color Color.Dracula.gray
        , padding 6
        , behindContent <| displayXp progress
        , clipY
        ]
    <|
        el [ centerX ] <|
            text <|
                String.fromInt progress.current
                    ++ "/"
                    ++ String.fromInt progress.max


displayXp : XpProgress -> Element msg
displayXp progress =
    progress.max
        |> List.range 1
        |> List.map
            (\xp ->
                if xp <= progress.current then
                    [ Background.color Color.Dracula.green
                    , Border.shadow
                        { offset = ( 0, 0 )
                        , blur = 6
                        , size = 3
                        , color = Color.Dracula.green
                        }
                    ]

                else
                    []
            )
        |> List.map (\attr -> el (attr ++ [ width fill, height (px 25) ]) none)
        |> row [ width fill ]


completed : XpProgress -> Bool
completed { current, max } =
    current >= max
