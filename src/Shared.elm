module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    )

import Core.Player as Player exposing (Player)
import Js.Events
import Json.Decode as Json
import Random
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { player : Player }


type Msg
    = UpdatePlayer Player
    | GotUpdatedPlayer (Result Json.Error Js.Events.Event)
    | GotHistory (Result Json.Error (List Js.Events.Event))


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { player = Player.unknown }, Random.generate UpdatePlayer Player.generator )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        UpdatePlayer player ->
            ( { model | player = player }
            , Js.Events.publish (Js.Events.Event player.id <| Js.Events.PlayerUpdated player)
            )

        GotUpdatedPlayer _ ->
            ( model, Cmd.none )

        GotHistory _ ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Js.Events.listenToOne GotUpdatedPlayer
        , Js.Events.listenToHistory GotHistory
        ]
