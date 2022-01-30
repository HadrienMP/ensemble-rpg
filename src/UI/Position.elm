module UI.Position exposing (..)

import Element exposing (..)
import Html.Attributes


type alias Position =
    { top : Maybe Int
    , right : Maybe Int
    , bottom : Maybe Int
    , left : Maybe Int
    }

emptyPosition : Position
emptyPosition = empty

empty : Position
empty =
    { top = Nothing
    , right = Nothing
    , left = Nothing
    , bottom = Nothing
    }


withBottom : Int -> Position -> Position
withBottom b p =
    { p | bottom = Just b }


withTop : Int -> Position -> Position
withTop a p =
    { p | top = Just a }


fixAt : Position -> Attribute msg
fixAt position =
    htmlAttribute <| Html.Attributes.attribute "style" <| "position: fixed; " ++ asCss position


asCss : Position -> String
asCss position =
    [ ( "top", position.top ), ( "right", position.right ), ( "bottom", position.bottom ), ( "left", position.left ) ]
        |> List.map
            (\( cssName, maybeValue ) ->
                case maybeValue of
                    Just value ->
                        cssName ++ ": " ++ String.fromInt value

                    Nothing ->
                        ""
            )
        |> String.join "; "
