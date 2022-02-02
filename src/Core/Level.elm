module Core.Level exposing (..)


type Level
    = Level1
    | Level2
    | Level3
    | Level4


all : List Level
all =
    next [] |> List.reverse


next : List Level -> List Level
next levels =
    case List.head levels of
        Nothing ->
            next [ Level1 ]

        Just Level1 ->
            next (Level2 :: levels)

        Just Level2 ->
            next (Level3 :: levels)

        Just Level3 ->
            next (Level4 :: levels)

        Just Level4 ->
            levels


previous : Level -> Maybe Level
previous level =
    case level of
        Level1 ->
            Nothing

        Level2 ->
            Just Level1

        Level3 ->
            Just Level2

        Level4 ->
            Just Level3


toString : Level -> String
toString level =
    case level of
        Level1 ->
            "1"

        Level2 ->
            "2"

        Level3 ->
            "3"

        Level4 ->
            "4"
