module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Color.Dracula
import Core.Profiles
import Element exposing (..)
import Element.Background as Background
import UI.Theme


type alias View msg =
    { title : String
    , body : Element msg
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = UI.Theme.container { profile = Core.Profiles.Player, currentRoute = Nothing } [] <| Element.text str
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = Element.map fn view.body
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title ++ " | Ensemble RPG"
    , body =
        [ layout [ Background.color Color.Dracula.black ] view.body
        ]
    }
