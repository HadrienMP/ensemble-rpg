module Pages.Player exposing (Model, Msg, page)

import Color.Dracula
import Core.Player exposing (Player)
import Core.Player.Event exposing (EventData(..))
import Core.RoleCard exposing (DisplayMode(..))
import Effect exposing (Effect)
import Element exposing (centerX, column, el, padding, paddingEach, paddingXY, px, row, spacing, text, width, wrappedRow)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Gen.Route
import Js.Storage
import Page
import Random
import Request exposing (Request)
import Shared
import UI.Icons
import UI.Theme exposing (CardSize(..), emptySides, transparent)
import Uuid exposing (Uuid, uuidGenerator)
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.protected.advanced <|
        \_ ->
            { init = init shared
            , update = update shared
            , view = view shared
            , subscriptions = subscriptions
            }



-- INIT


type alias Model =
    { name : String
    , nextEventId : Maybe Uuid
    }


empty : Model
empty =
    { name = ""
    , nextEventId = Nothing
    }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { empty | name = shared.player.identity.name }
    , Effect.fromCmd <| Random.generate GotUuid uuidGenerator
    )



-- UPDATE


type Msg
    = SaveName Uuid
    | NameChanged String
    | GotUuid Uuid


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update { player } msg model =
    case msg of
        NameChanged name ->
            ( { model | name = String.slice 0 10 name }, Effect.none )

        SaveName uuid ->
            let
                identity =
                    player.identity

                updatedIdentity =
                    { identity | name = String.slice 0 10 model.name }
            in
            ( { model | nextEventId = Nothing }
            , Effect.batch
                [ ChangedIdentity updatedIdentity
                    |> Core.Player.Event.toEvent uuid player.identity.id
                    |> Shared.PlayerEvent
                    |> Effect.fromShared
                , Js.Storage.saveIdentity updatedIdentity
                    |> Effect.fromCmd
                , Effect.fromCmd <| Random.generate GotUuid uuidGenerator
                ]
            )

        GotUuid uuid ->
            ( { model | nextEventId = Just uuid }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view { player, profile } model =
    { title = "User"
    , body =
        UI.Theme.container
            { profile = profile
            , currentRoute = Just Gen.Route.Player
            }
            []
        <|
            column [ centerX, spacing 20 ]
                [ playerCard player model
                , wrappedRow [ spacing 10, centerX ] <|
                    displayWonBadges player
                ]
    }


displayWonBadges : Player -> List (Element.Element Msg)
displayWonBadges player =
    Core.Player.completedRoleCards player
        |> List.map (Core.RoleCard.view Badge)


playerCard : Player -> Model -> Element.Element Msg
playerCard player model =
    UI.Theme.card []
        { icon =
            el [ Element.Font.size 60, centerX ] <|
                text <|
                    String.fromChar player.identity.icon
        , color = Color.Dracula.green
        , size = Big
        , main =
            row
                [ paddingEach
                    { emptySides
                        | bottom = 4
                        , left = 20
                        , right = 20
                    }
                , centerX
                ]
            <|
                [ nameInput model
                , saveButton model
                ]
        , sub =
            Core.Player.numberOfBadgesWon player
                |> String.fromInt
                |> (\a -> a ++ " badges")
                |> text
        }


nameInput : Model -> Element.Element Msg
nameInput model =
    Element.Input.text
        [ Element.Background.color <| transparent 10 Color.Dracula.green
        , Element.Border.rounded 0
        , Element.Border.widthEach { emptySides | bottom = 1 }
        , Element.Border.color Color.Dracula.white
        , Element.Font.size 20
        , width (px 100)
        , paddingXY 0 4
        , Element.Font.shadow { offset = ( 1, 1 ), blur = 2, color = Color.Dracula.gray }
        ]
        { onChange = NameChanged
        , text = model.name
        , placeholder = Nothing
        , label = Element.Input.labelHidden "Your name"
        }


saveButton : Model -> Element.Element Msg
saveButton model =
    Element.Input.button
        [ Element.Border.solid
        , Element.Border.color Color.Dracula.white
        , Element.Border.width 1
        , Element.Background.color Color.Dracula.pink
        , padding 4
        ]
        { onPress = Maybe.map SaveName model.nextEventId
        , label = el [ width <| px 20 ] UI.Icons.save
        }
