module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import AssocList exposing (Dict)
import Core.Player as Player exposing (Player)
import Dict
import Json.Decode as Json
import Request exposing (Request)
import Core.Role exposing (Role)


type alias Flags =
    Json.Value


type alias Model =
    { player : Player }


type Msg
    = XpChanged Int Role


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { player = Player.unknown }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        XpChanged xp role ->
            ( {model | player = Player.updateXp xp role model.player}, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
