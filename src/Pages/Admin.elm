module Pages.Admin exposing (Model, Msg, page)

import Color.Dracula
import Core.Room exposing (Room)
import Element exposing (centerX, column, el, fill, padding, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Gen.Params.Admin exposing (Params)
import Gen.Route
import Js.Events
import Page
import Request
import Shared
import UI.Icons
import UI.Theme
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.protected.element <|
        \room ->
            { init = init
            , update = update room
            , view = view shared
            , subscriptions = subscriptions
            }



-- INIT


type alias Model =
    { resetRequested : Bool }


init : ( Model, Cmd Msg )
init =
    ( { resetRequested = False }, Cmd.none )



-- UPDATE


type Msg
    = ResetRequested
    | ResetConfirmed


update : Room -> Msg -> Model -> ( Model, Cmd Msg )
update room msg model =
    case msg of
        ResetRequested ->
            ( { model | resetRequested = True }, Cmd.none )

        ResetConfirmed ->
            ( { model | resetRequested = False }, Js.Events.publish { room = room, content = Js.Events.Reset } )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Homepage"
    , body =
        UI.Theme.container
            { profile = shared.profile
            , currentRoute = Just Gen.Route.Admin
            }
            []
        <|
            column [ width fill ] [ resetButton model ]
    }


resetButton model =
    if model.resetRequested then
        button
            [ Background.color Color.Dracula.red
            , Border.rounded 10
            , padding 10
            , width fill
            , Border.solid
            , Border.color Color.Dracula.white
            , Border.width 1
            , Font.shadow
                { offset = ( 0, 0 )
                , blur = 4
                , color = Color.Dracula.black
                }
            ]
            { onPress = Just ResetConfirmed
            , label =
                row [ spacing 10, centerX ]
                    [ el [ width <| px 20 ] <| UI.Icons.checkMark Color.Dracula.white
                    , text "Confirm ?"
                    ]
            }

    else
        button
            [ Background.color Color.Dracula.orange
            , Border.rounded 10
            , padding 10
            , width fill
            , Border.solid
            , Border.color Color.Dracula.white
            , Border.width 1
            , Font.shadow
                { offset = ( 0, 0 )
                , blur = 4
                , color = Color.Dracula.black
                }
            ]
            { onPress = Just ResetRequested
            , label =
                row [ spacing 10, centerX ]
                    [ el [ width <| px 20 ] UI.Icons.trash
                    , text "Reset"
                    ]
            }
