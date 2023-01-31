module Core.Room exposing (..)


type Room
    = Room String


fromString : String -> Room
fromString =
    Room


print : Room -> String
print (Room name) =
    name
