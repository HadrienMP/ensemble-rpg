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
import Core.Room exposing (Room)
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
    { room : Maybe Room
    , player : Player
    , players : Dict PlayerId Player
    , profile : Core.Profiles.Profile
    , processedPlayerEvents : List Uuid
    }


empty : Model
empty =
    { room = Nothing
    , players = Dict.empty
    , player = Player.unknown
    , profile = Core.Profiles.Player
    , processedPlayerEvents = []
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


evolveMany : List Js.Events.Super -> Request -> Model -> Model
evolveMany events req model =
    case events of
        [] ->
            model

        head :: tail ->
            evolve model req head
                |> Tuple.first
                |> evolveMany tail req


evolve : Model -> Request -> Js.Events.Super -> ( Model, Cmd Msg )
evolve model req event =
    if model.room == Just event.room then
        case event.content of
            Js.Events.PlayerEvent playerEvent ->
                if playerEvent.playerId == model.player.identity.id then
                    evolveCurrentPlayer playerEvent model req

                else
                    ( { model
                        | players =
                            Dict.update (Debug.log "event for " playerEvent.playerId)
                                (\maybePlayer ->
                                    maybePlayer
                                        |> Maybe.withDefault Player.unknown
                                        |> Player.evolve playerEvent.data
                                        |> Just
                                )
                                model.players
                      }
                    , Cmd.none
                    )

            Js.Events.Reset ->
                ( { model | player = Player.reset model.player, players = Dict.empty }
                , Cmd.none
                )

    else
        ( model, Cmd.none )


evolveCurrentPlayer : Core.Player.Event.Event -> Model -> Request -> ( Model, Cmd Msg )
evolveCurrentPlayer playerEvent model req =
    if List.member playerEvent.id model.processedPlayerEvents then
        ( model, Cmd.none )

    else
        let
            updated =
                Player.evolve playerEvent.data model.player

            redirectionCommand =
                if updated.completedRoles == model.player.completedRoles then
                    Cmd.none

                else
                    Request.replaceRoute Gen.Route.Team req
        in
        ( { model
            | player = updated
            , processedPlayerEvents = playerEvent.id :: model.processedPlayerEvents
          }
        , redirectionCommand
        )



-- Msg


type Msg
    = PlayerEvent Core.Player.Event.Event
    | CreatedPlayer ( Uuid, Player )
    | GotEvent (Result Json.Error Js.Events.Super)
    | GotHistory (Result Json.Error (List Js.Events.Super))
    | SelectRoom Uuid Room
    | QuitRoom



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
            ( { empty | profile = Core.Profiles.fromAdminBool flags.admin }
            , ( uuidGenerator, Player.generator )
                |> (\( a, b ) -> Random.map2 Tuple.pair a b)
                |> Random.generate CreatedPlayer
            )

        Just playerIdentity ->
            ( { empty
                | player = Player.fromIdentity playerIdentity
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
            case model.room of
                Just room ->
                    evolveCurrentPlayer event model req
                        |> Tuple.mapSecond List.singleton
                        |> Tuple.mapSecond
                            ((::) <|
                                Js.Events.publish
                                    { room = room
                                    , content = Js.Events.PlayerEvent event
                                    }
                            )
                        |> Tuple.mapSecond Cmd.batch

                Nothing ->
                    evolveCurrentPlayer event model req

        CreatedPlayer ( uuid, player ) ->
            case model.room of
                Just room ->
                    ( { model | player = player }
                    , Cmd.batch
                        [ Js.Storage.saveIdentity player.identity
                        , Js.Events.publish
                            { room = room
                            , content =
                                ChangedIdentity player.identity
                                    |> toEvent uuid player.identity.id
                                    |> Js.Events.PlayerEvent
                            }
                        ]
                    )

                Nothing ->
                    ( { model | player = player }
                    , Cmd.batch
                        [ Js.Storage.saveIdentity player.identity
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

        SelectRoom uuid room ->
            ( { model | room = Just room }
            , Cmd.batch
                [ Request.pushRoute Gen.Route.Home_ req
                , Js.Events.join { room = Core.Room.print room }
                , Js.Events.publish
                    { room = room
                    , content =
                        ChangedIdentity model.player.identity
                            |> toEvent uuid model.player.identity.id
                            |> Js.Events.PlayerEvent
                    }
                ]
            )

        QuitRoom ->
            ( { model | room = Nothing }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Js.Events.listenToOne GotEvent
        , Js.Events.listenToHistory GotHistory
        ]
