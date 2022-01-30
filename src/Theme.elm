module Theme exposing (..)

import Color.Dracula
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Region as Region


container : List (Element msg) -> Element msg
container children =
    column
        [ Background.color Color.Dracula.black
        , Font.color Color.Dracula.white
        , Font.size 14
        , width <| maximum 2000 fill
        , height fill
        ]
    <|
        [ el
            [ Background.color Color.Dracula.blue
            , width fill
            , padding 10
            , padding 10
            , Font.size 22
            ]
          <|
            text "Mob RPG"
        , column
            [ paddingXY 10 20
            , spacingXY 0 20
            , centerX
            ]
            children
        ]


h1 : List (Attribute msg)
h1 =
    [ Region.heading 1
    , Font.color Color.Dracula.white
    , Font.size 20
    ]


h2 : List (Attribute msg)
h2 =
    [ Region.heading 2
    , Font.color Color.Dracula.white
    , Font.size 18
    ]


darken : Int -> Color -> Color
darken amount color =
    toRgb color
        |> (\a ->
                { a
                    | red = a.red * toFactor amount
                    , green = a.green * toFactor amount
                    , blue = a.blue * toFactor amount
                }
           )
        |> fromRgb


transparent : Int -> Color -> Color
transparent amount color =
    toRgb color
        |> (\a ->
                { a
                    | alpha = a.alpha * toFactor amount
                }
           )
        |> fromRgb


toFactor : Int -> Float
toFactor amount =
    toFloat (10 - amount) / 10
