module Pages.Player exposing (Model, Msg, page)

import Color.Dracula
import Core.RoleCard exposing (DisplayMode(..))
import Effect exposing (Effect)
import Element exposing (centerX, column, el, paddingEach, paddingXY, px, spacing, text, width, wrappedRow)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Gen.Route
import Js.Storage
import Page
import Request exposing (Request)
import Shared
import UI.Theme exposing (CardSize(..), emptySides, transparent)
import View exposing (View)
import Core.Player.Event exposing (Event(..))
import Core.Player


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = NameChanged String


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update { player } msg _ =
    case msg of
        NameChanged name ->
            let
                identity =
                    player.identity

                updatedIdentity =
                    { identity | name = String.slice 0 10 name }
            in
            ( {}
            , Effect.batch
                [ ChangedIdentity updatedIdentity
                    |> Shared.PlayerEvent player.identity.id
                    |> Effect.fromShared
                , Js.Storage.saveIdentity updatedIdentity
                    |> Effect.fromCmd
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view { player, profile } _ =
    { title = "User"
    , body =
        UI.Theme.container { profile = profile, currentRoute = Just Gen.Route.Player } [] <|
            column [ centerX, spacing 20 ]
                [ UI.Theme.card []
                    { icon = el [ Element.Font.size 60, centerX ] <| text <| String.fromChar player.identity.icon
                    , color = Color.Dracula.green
                    , size = Big
                    , main =
                        el [ paddingEach { emptySides | bottom = 4, left = 20, right = 20 } ] <|
                            Element.Input.text
                                [ Element.Background.color <| transparent 10 Color.Dracula.green
                                , Element.Border.rounded 0
                                , Element.Border.widthEach { emptySides | bottom = 1 }
                                , Element.Font.size 20
                                , width (px 100)
                                , paddingXY 0 4
                                , Element.Font.shadow { offset = ( 1, 1 ), blur = 2, color = Color.Dracula.gray }
                                ]
                                { onChange = NameChanged
                                , text = player.identity.name
                                , placeholder = Nothing
                                , label = Element.Input.labelHidden "Your name"
                                }
                    , sub = text <| (String.fromInt <| Core.Player.numberOfBadgesWon player) ++ " badges"
                    }
                , wrappedRow [ spacing 10, centerX ]
                    (Core.Player.completedRoleCards player
                        |> List.map (Core.RoleCard.view Badge)
                    )
                ]
    }
