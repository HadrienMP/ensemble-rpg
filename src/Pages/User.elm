module Pages.User exposing (Model, Msg, page)

import Color.Dracula
import Core.Player exposing (Player)
import Effect exposing (Effect)
import Element exposing (centerX, centerY, column, el, fill, padding, shrink, spacingXY, text, width)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Gen.Params.User exposing (Params)
import Page
import Request
import Shared
import UI.Theme exposing (emptySides, h1)
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
            ( {model | player = Core.Player.updateName name model.player}, Effect.fromShared <| Shared.NameChanged name )



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
            column
                [ width shrink
                , centerY
                , centerX
                , Element.Border.rounded 10
                , Element.Border.width 1
                , Element.Border.solid
                , Element.Border.color Color.Dracula.gray
                ]
                [ h1
                    [ width fill
                    , Element.Border.widthEach { emptySides | bottom = 1 }
                    , Element.Border.solid
                    , Element.Border.color Color.Dracula.gray
                    , padding 20
                    ]
                    (text "Profile")
                , el [ padding 20 ] <|
                    Element.Input.text
                        [ spacingXY 20 0
                        , Element.Background.color Color.Dracula.black
                        , Element.Border.rounded 0
                        , Element.Border.widthEach { emptySides | bottom = 1 }
                        , Element.Font.size 20
                        ]
                        { onChange = NameChanged
                        , text = model.player.name
                        , placeholder = Nothing
                        , label = Element.Input.labelLeft [ Element.Font.size 14 ] <| text "Your name"
                        }
                ]
    }
