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
import Core.Player.Event exposing (EventData(..), toEvent)
import Core.Player.Id exposing (PlayerId)
import Core.Profiles
import Gen.Route
import Js.Events
import Js.Storage
import Json.Decode as Json
import List exposing (head)
import Random
import Request exposing (Request)
import Uuid exposing (Uuid, uuidGenerator)


type alias Flags =
    { storage : String
    , admin : Bool
    }



-- Model


type alias Model =
    { player : Player
    , players : Dict PlayerId Player
    , profile : Core.Profiles.Profile
    }


empty : Model
empty =
    { players = Dict.empty
    , player = Player.unknown
    , profile = Core.Profiles.Player
    }


score : Model -> Int
score model =
    Dict.values model.players
        |> (::) model.player
        |> List.map (.completedRoles >> Dict.size)
        |> List.sum


allPlayers : Model -> List Player
allPlayers model =
    model.player :: Dict.values model.players


evolveMany : List Js.Events.Event -> Request -> Model -> Model
evolveMany events req model =
    case events of
        [] ->
            model

        head :: tail ->
            evolve model req head
                |> Tuple.first
                |> evolveMany tail req


evolve : Model -> Request -> Js.Events.Event -> ( Model, Cmd Msg )
evolve model req event =
    case event of
        Js.Events.PlayerEvent playerEvent ->
            if playerEvent.playerId == model.player.identity.id then
                let
                    updated =
                        Player.evolve playerEvent.data model.player

                    redirectionCommand =
                        if updated.completedRoles == model.player.completedRoles then
                            Cmd.none

                        else
                            Request.replaceRoute Gen.Route.Team req
                in
                ( { model | player = updated }
                , redirectionCommand
                )

            else
                ( { model
                    | players =
                        Dict.update playerEvent.playerId
                            (\a -> Maybe.withDefault Player.unknown a |> Player.evolve playerEvent.data |> Just)
                            model.players
                  }
                , Cmd.none
                )

        Js.Events.Reset ->
            ( { model | player = Player.reset model.player, players = Dict.empty }
            , Cmd.none
            )



-- Msg


type Msg
    = PlayerEvent Core.Player.Event.Event
    | CreatedPlayer ( Uuid, Player )
    | GotEvent (Result Json.Error Js.Events.Event)
    | GotHistory (Result Json.Error (List Js.Events.Event))



-- Init


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
            , ( uuidGenerator, Player.generator )
                |> (\( a, b ) -> Random.map2 Tuple.pair a b)
                |> Random.generate CreatedPlayer
            )

        Just playerIdentity ->
            ( { players = Dict.empty
              , player = Player.fromIdentity playerIdentity
              , profile = Core.Profiles.fromAdminBool flags.admin
              }
            , uuidGenerator
                |> Random.map (\a -> ( a, Player.fromIdentity playerIdentity ))
                |> Random.generate CreatedPlayer
            )



-- Update


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        PlayerEvent event ->
            ( model
            , Js.Events.publish <| Js.Events.PlayerEvent event
            )

        CreatedPlayer ( uuid, player ) ->
            ( { model | player = player }
            , Cmd.batch
                [ Js.Storage.saveIdentity player.identity
                , ChangedIdentity player.identity
                    |> toEvent uuid player.identity.id
                    |> Js.Events.PlayerEvent
                    |> Js.Events.publish
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


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Js.Events.listenToOne GotEvent
        , Js.Events.listenToHistory GotHistory
        ]
