module Pages.Team exposing (Model, Msg, page)

import Color.Dracula
import Core.Player as Player exposing (Player)
import Core.RoleCard as RoleCard exposing (DisplayMode(..))
import Element exposing (column, el, paddingEach, row, spacing, spacingXY, text, wrappedRow)
import Gen.Params.Team exposing (Params)
import Page
import Request
import Shared
import UI.Icons
import UI.Theme exposing (CardSize(..), container, emptySides, h2)
import View exposing (View)
import Element exposing (width)
import Element exposing (fill)
import Element exposing (shrink)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init shared
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { player : Player
    }


init : Shared.Model -> ( Model, Cmd Msg )
init model =
    ( { player = model.player }, Cmd.none )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Team"
    , body =
        container [] <|
            column [ spacingXY 0 20 ]
                [ row [ spacing 10 ]
                    [ UI.Theme.card [width shrink]
                        { icon = UI.Icons.person
                        , color = Color.Dracula.green
                        , size = Small
                        , main = text model.player.name
                        , sub = text <| (String.fromInt <| Player.numberOfBadgesWon model.player) ++ " badges"
                        }
                    , wrappedRow [ spacing 10, width fill ]
                        (Player.badgesWon model.player
                            |> List.map (RoleCard.view Badge)
                        )
                    ]
                ]
    }
