module Domain.Level exposing (..)


type Level
    = Level1
    | Level2
    | Level3
    | Level4


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
