module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , allPlayers
    , empty
    , init
    , score
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
import Gen.Route


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
    { players = Dict.empty
    , player = Player.unknown
    , profile = Core.Profiles.Player
    }


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
            , Cmd.batch
                [ Js.Events.publish <| Js.Events.PlayerEvent player.id <| Player.ChangedIdentity player.identity
                , Js.Events.ready ()
                ]
            )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        PlayerEvent playerId event ->
            ( model
            , Js.Events.publish <| Js.Events.PlayerEvent playerId event
            )

        CreatedPlayer { player, event } ->
            ( { model | player = player }
            , Cmd.batch
                [ Js.Storage.saveIdentity player.id player.identity
                , Js.Events.publish <| Js.Events.PlayerEvent player.id event
                , Js.Events.ready ()
                ]
            )

        GotEvent result ->
            result
                |> Result.map (evolve model req)
                |> Result.withDefault ( model, Cmd.none )

        GotHistory result ->
            ( result
                |> Result.map (\events -> evolveMany events req model)
                |> Result.withDefault model
            , Cmd.none
            )


score : Model -> Int
score model =
    Dict.values model.players
        |> (::) model.player
        |> List.map (.completedRoles >> Dict.size)
        |> List.sum


evolveMany : List Event -> Request -> Model -> Model
evolveMany events req model =
    case events of
        [] ->
            model

        head :: tail ->
            evolve model req head
                |> Tuple.first
                |> evolveMany tail req


evolve : Model -> Request -> Event -> ( Model, Cmd Msg )
evolve model req event =
    case event of
        Js.Events.PlayerEvent playerId playerEvent ->
            if playerId == model.player.id then
                let
                    { updated, events } =
                        Player.evolve playerEvent model.player
                    redirectionCommand = 
                        case playerEvent of
                            Player.CompletedRole _ -> Request.replaceRoute Gen.Route.Team req
                            _ -> Cmd.none
                in
                ( { model | player = updated }
                , events
                    |> List.map (Js.Events.PlayerEvent updated.id)
                    |> List.map Js.Events.publish
                    |> (::) redirectionCommand
                    |> Cmd.batch
                )

            else
                ( { model
                    | players =
                        Dict.update playerId
                            (\a -> Maybe.withDefault Player.unknown a |> Player.evolve playerEvent |> .updated |> Just)
                            model.players
                  }
                , Cmd.none
                )

        Js.Events.Reset ->
            ( { model | player = Player.reset model.player, players = Dict.empty }
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Js.Events.listenToOne GotEvent
        , Js.Events.listenToHistory GotHistory
        ]
