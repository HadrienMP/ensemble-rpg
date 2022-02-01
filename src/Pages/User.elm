module Pages.User exposing (Model, Msg, page)

import Color.Dracula
import Core.Player as Player exposing (Player)
import Core.RoleCard exposing (DisplayMode(..))
import Effect exposing (Effect)
import Element exposing (centerX, column, el, paddingEach, paddingXY, px, spacing, text, width, wrappedRow)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Gen.Params.User exposing (Params)
import Page
import Request
import Shared
import UI.Icons
import UI.Theme exposing (CardSize(..), emptySides, transparent)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { player : Player }


init : Shared.Model -> ( Model, Effect Msg )
init shared =
    ( { player = shared.player }, Effect.none )



-- UPDATE


type Msg
    = NameChanged String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NameChanged name ->
            ( { model | player = Player.updateName name model.player }, Effect.fromShared <| Shared.NameChanged name )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "User"
    , body =
        UI.Theme.container [] <|
            column [ centerX, spacing 20 ]
                [ UI.Theme.card []
                    { icon = UI.Icons.person
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
                                , text = model.player.name
                                , placeholder = Nothing
                                , label = Element.Input.labelHidden "Your name"
                                }
                    , sub = text <| (String.fromInt <| Player.numberOfBadgesWon model.player) ++ " badges"
                    }
                , wrappedRow [ spacing 10, centerX ]
                    (Player.badgesWon model.player
                        |> List.map (Core.RoleCard.view Badge)
                    )
                ]
    }
