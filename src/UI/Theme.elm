module UI.Theme exposing (..)

import Color.Dracula
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Gen.Route as Route
import UI.Icons as Icon
import UI.Position as Position exposing (emptyPosition)


maxWidthBody : Int
maxWidthBody =
    800


container : Element msg -> Element msg
container children =
    el
        [ inFront navigation
        , behindContent header
        , Font.color Color.Dracula.white
        , Font.size 14
        , width fill
        , paddingEach { top = 44, left = 0, right = 0, bottom = 60 }
        ]
    <|
        el
            [ padding 10
            , width <| maximum maxWidthBody fill
            , centerX
            , Region.mainContent
            ]
            children


header : Element msg
header =
    el
        [ paddingXY 20 16
        , Font.size 20
        , Font.color <| darken 4 <| Color.Dracula.white
        , Font.italic
        , Font.light
        , alignRight
        , Position.fixAt { emptyPosition | top = Just 0 }
        ]
    <|
        text "Ensemble"


navigation : Element msg
navigation =
    el
        [ Region.navigation
        , Background.color Color.Dracula.blue
        , width fill
        , Position.fixAt { emptyPosition | bottom = Just 0 }
        , Border.shadow
            { offset = ( 0, 0 )
            , blur = 2
            , size = 2
            , color = Color.Dracula.black |> transparent 3
            }
        ]
    <|
        row [ width <| maximum maxWidthBody <| fill, centerX ]
            [ navLink [ Border.widthEach { emptySides | left = 1, right = 1 } ]
                { route = Route.Home_, label = el [ width <| px 30, centerX ] Icon.comedyMasks }
            , navLink []
                { route = Route.Team, label = el [ width <| px 30, centerX ] Icon.ribbon }
            ]


navLink : List (Attribute msg) -> { route : Route.Route, label : Element msg } -> Element msg
navLink attributes description =
    link
        ([ Border.solid
         , Border.color Color.Dracula.black
         , Border.widthEach { emptySides | right = 1 }
         , paddingXY 10 10
         , width fill
         ]
            ++ attributes
        )
        { url = Route.toHref description.route, label = description.label }


emptySides : { top : Int, left : Int, right : Int, bottom : Int }
emptySides =
    { top = 0, left = 0, right = 0, bottom = 0 }


h1 : List (Attribute msg) -> Element msg -> Element msg
h1 attributes =
    el <|
        [ Region.heading 1
        , Font.color Color.Dracula.white
        , Font.size 20
        ]
            ++ attributes


h2 : List (Attribute msg) -> Element msg -> Element msg
h2 attributes =
    el <|
        [ Region.heading 2
        , Font.color Color.Dracula.white
        , Font.size 18
        , Font.color Color.Dracula.green
        , paddingEach { emptySides | bottom = 20 }
        ]
            ++ attributes


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
