module Core.Profiles exposing (..)


type Profile
    = Player
    | Admin

fromAdminBool : Bool -> Profile
fromAdminBool admin =
    if admin then
        Admin
    else
        Player