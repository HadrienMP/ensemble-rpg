module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Core.Player as Player exposing (Player)
import Core.Role exposing (Role)
import Json.Decode as Json
import Random
import Request exposing (Request)
import Core.Animals


type alias Flags =
    Json.Value


type alias Model =
    { player : Player }


type Msg
    = XpChanged Int Role
    | UpdatePlayer Player


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { player = Player.unknown }, Random.generate UpdatePlayer Player.generator )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        UpdatePlayer player ->
            ( { model | player = player }, Cmd.none )

        XpChanged xp role ->
            ( { model | player = Player.updateXp xp role model.player }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
