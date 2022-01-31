module Core.Player exposing (..)

import AssocList as Dict exposing (Dict)
import Core.Role exposing (Role)


type alias Player =
    { name : String
    , xp : Dict Role Int
    }


unknown : Player
unknown =
    { name = "Unknown", xp = Dict.empty }


xpOf : Role -> Player -> Int
xpOf role player =
    Dict.get role player.xp
        |> Maybe.withDefault 0


updateXp : Int -> Role -> Player -> Player
updateXp xp role player =
    { player | xp = Dict.insert role xp player.xp }
