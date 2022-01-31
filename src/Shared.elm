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
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { player : Player }


type Msg
    = XpChanged Int Role
    | NameChanged String


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { player = Player.unknown }, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        XpChanged xp role ->
            ( { model | player = Player.updateXp xp role model.player }, Cmd.none )

        NameChanged name ->
            ( { model | player = Player.updateName name model.player }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
