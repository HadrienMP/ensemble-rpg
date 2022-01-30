module Core.Player exposing (..)
import Dict exposing (Dict)
import Core.Role exposing (Role)


type Xp
    = Xp Int


type alias Player =
    { name : String
    , xp : Dict Role Xp
    }
    