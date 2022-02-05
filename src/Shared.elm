module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , init
    , subscriptions
    , update
    , allPlayers
    )

import AssocList as Dict exposing (Dict)
import Core.OtherPlayer exposing (OtherPlayer)
import Core.Player as Player exposing (Player)
import Core.PlayerId exposing (PlayerId)
import Js.Events exposing (Event, EventDetails(..))
import Json.Decode as Json
import List exposing (head)
import Random
import Request exposing (Request)


type alias Flags =
    Json.Value


type alias Model =
    { player : Player
    , otherPlayers : Dict PlayerId OtherPlayer
    }

allPlayers : Model -> List OtherPlayer
allPlayers model =
    (Core.OtherPlayer.fromPlayer model.player) :: (Dict.values model.otherPlayers)

type Msg
    = UpdatePlayer Player
    | GotUpdatedPlayer (Result Json.Error Js.Events.Event)
    | GotHistory (Result Json.Error (List Js.Events.Event))


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( { player = Player.unknown
      , otherPlayers = Dict.empty
      }
    , Random.generate UpdatePlayer Player.generator
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        UpdatePlayer player ->
            ( { model | player = player }
            , Core.OtherPlayer.fromPlayer player
                |> Js.Events.PlayerUpdated
                |> Js.Events.Event player.id
                |> Js.Events.publish
            )

        GotUpdatedPlayer result ->
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
    case event.details of
        PlayerUpdated otherPlayer ->
            if otherPlayer.id == model.player.id then
                model

            else
                { model | otherPlayers = Dict.insert event.playerId otherPlayer model.otherPlayers }


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ Js.Events.listenToOne GotUpdatedPlayer
        , Js.Events.listenToHistory GotHistory
        ]
