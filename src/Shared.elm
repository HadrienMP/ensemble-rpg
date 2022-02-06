module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , allPlayers
    , init
    , subscriptions
    , update
    )

import AssocList as Dict exposing (Dict)
import Core.Player as Player exposing (Player)
import Core.PlayerId exposing (PlayerId)
import Js.Events exposing (Event)
import Js.Storage
import Json.Decode as Json
import List exposing (head)
import Random
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { player : Player
    , players : Dict PlayerId Player
    }


allPlayers : Model -> List Player
allPlayers model =
    model.player :: Dict.values model.players


type Msg
    = PlayerEvent PlayerId Player.Event
    | CreatedPlayer Player.PlayerWithIdentityEvent
    | GotEvent (Result Json.Error Js.Events.Event)
    | GotHistory (Result Json.Error (List Js.Events.Event))


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { players = Dict.empty, player = Player.unknown }
    , Random.generate CreatedPlayer Player.generator
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        PlayerEvent playerId event ->
            ( { model | player = Player.evolve event model.player }
            , Js.Events.publish <| Js.Events.Event playerId event
            )

        CreatedPlayer { player, event } ->
            ( { model | player = player }
            , Cmd.batch
                [ Js.Storage.save player.id player.identity
                , Js.Events.publish <| Js.Events.Event player.id event
                ]
            )

        GotEvent result ->
            ( result
                |> Result.map (evolve model)
                |> Result.withDefault model
            , Cmd.none
            )

        GotHistory result ->
            ( result
                |> Result.map (evolveMany model)
                |> Result.withDefault model
            , Cmd.none
            )


evolveMany : Model -> List Event -> Model
evolveMany model events =
    case events of
        [] ->
            model

        head :: tail ->
            evolveMany (evolve model head) tail


evolve : Model -> Event -> Model
evolve model { playerId, playerEvent } =
    if playerId == model.player.id then
        model

    else
        { model
            | players =
                Dict.update playerId
                    (\a -> Maybe.withDefault Player.unknown a |> Player.evolve playerEvent |> Just)
                    model.players
        }


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Js.Events.listenToOne GotEvent
        , Js.Events.listenToHistory GotHistory
        ]
