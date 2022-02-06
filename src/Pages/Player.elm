module Pages.Player exposing (Model, Msg, page)

import Color.Dracula
import Core.Player as Player exposing (Event(..))
import Core.RoleCard exposing (DisplayMode(..))
import Effect exposing (Effect)
import Element exposing (centerX, column, el, paddingEach, paddingXY, px, spacing, text, width, wrappedRow)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Page
import Request exposing (Request)
import Shared
import UI.Theme exposing (CardSize(..), emptySides, transparent)
import View exposing (View)


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
    let
        identity =
            player.identity
    in
    case msg of
        NameChanged name ->
            ( {}
            , ChangedIdentity { identity | name = name }
                |> Shared.PlayerEvent player.id
                |> Effect.fromShared
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
        let
            identity =
                player.identity
        in
        UI.Theme.container profile [] <|
            column [ centerX, spacing 20 ]
                [ UI.Theme.card []
                    { icon = el [ Element.Font.size 60, centerX ] <| text <| String.fromChar identity.icon
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
                                , text = identity.name
                                , placeholder = Nothing
                                , label = Element.Input.labelHidden "Your name"
                                }
                    , sub = text <| (String.fromInt <| Player.numberOfBadgesWon player) ++ " badges"
                    }
                , wrappedRow [ spacing 10, centerX ]
                    (Player.completedRoleCards player
                        |> List.map (Core.RoleCard.view Badge)
                    )
                ]
    }
