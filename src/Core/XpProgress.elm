module Core.XpProgress exposing (..)
import Core.Role exposing (Role)

type alias XpProgress =
    { current : Int
    , max : Int
    , role : Role
    }

completed : XpProgress -> Bool
completed {current, max} =
    current >= max
