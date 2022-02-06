module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , allPlayers
    , empty
    , init
    , subscriptions
    , update
    )

import AssocList as Dict exposing (Dict)
import Core.Player as Player exposing (Player)
import Core.PlayerId exposing (PlayerId)
import Core.Profiles
import Js.Events exposing (Event)
import Js.Storage
import Json.Decode as Json
import List exposing (head)
import Random
import Request exposing (Request)


type alias Flags =
    { storage : String
    , admin : Bool
    }


type alias Model =
    { player : Player
    , players : Dict PlayerId Player
    , profile : Core.Profiles.Profile
    }


allPlayers : Model -> List Player
allPlayers model =
    model.player :: Dict.values model.players


type Msg
    = PlayerEvent PlayerId Player.Event
    | CreatedPlayer Player.PlayerWithIdentityEvent
    | GotEvent (Result Json.Error Js.Events.Event)
    | GotHistory (Result Json.Error (List Js.Events.Event))


empty : Model
empty =
    { players = Dict.empty, player = Player.unknown, profile = Core.Profiles.Player }


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    let
        savedPlayer =
            flags.storage
                |> Js.Storage.fromString
                |> .player
    in
    case savedPlayer of
        Nothing ->
            ( { players = Dict.empty
              , player = Player.unknown
              , profile = Core.Profiles.fromAdminBool flags.admin
              }
            , Random.generate CreatedPlayer Player.generator
            )

        Just player ->
            ( { players = Dict.empty
              , player = Player.fromIdentity player.id player.identity
              , profile = Core.Profiles.fromAdminBool flags.admin
              }
            , Js.Events.ready ()
            )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        PlayerEvent playerId event ->
            ( { model | player = Player.evolve event model.player }
            , Js.Events.publish <| Js.Events.PlayerEvent playerId event
            )

        CreatedPlayer { player, event } ->
            ( { model | player = player }
            , Cmd.batch
                [ Js.Storage.persist { player = Just { id = player.id, identity = player.identity } }
                , Js.Events.publish <| Js.Events.PlayerEvent player.id event
                , Js.Events.ready ()
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
evolve model event =
    case event of
        Js.Events.PlayerEvent playerId playerEvent ->
            if playerId == model.player.id then
                { model | player = Player.evolve playerEvent model.player }

            else
                { model
                    | players =
                        Dict.update playerId
                            (\a -> Maybe.withDefault Player.unknown a |> Player.evolve playerEvent |> Just)
                            model.players
                }

        Js.Events.Reset ->
            { model | player = Player.reset model.player, players = Dict.empty }


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Js.Events.listenToOne GotEvent
        , Js.Events.listenToHistory GotHistory
        ]
