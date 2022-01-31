module Pages.Role.Id_ exposing (Model, Msg, page)

import Color.Dracula
import Core.Player as Player
import Core.Role
import Core.RoleCard as RoleCard exposing (Behaviour, RoleCard)
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Role.Id_ exposing (Params)
import Gen.Route
import Page
import Request
import Shared
import UI.Theme as Theme exposing (h2)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init shared req
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- ###########################################
-- Model
-- ###########################################


type Msg
    = GainXp


type alias Model =
    { card : RoleCard Msg
    , xp : Int
    }



-- ###########################################
-- Init
-- ###########################################


init : Shared.Model -> Request.With Params -> ( Model, Effect Msg )
init shared req =
    case RoleCard.findById req.params.id of
        Just card ->
            ( { card = card, xp = Player.xpOf card.role shared.player }
            , Effect.none
            )

        Nothing ->
            ( { card = RoleCard.fromRole Core.Role.Mobber, xp = 0 }
            , Effect.fromCmd <| Request.replaceRoute Gen.Route.NotFound req
            )



-- ###########################################
-- Update
-- ###########################################


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GainXp ->
            let
                newXp =
                    RoleCard.incXp model.xp model.card
            in
            
            ( { model | xp = newXp }
            , Effect.fromShared <| Shared.XpChanged newXp model.card.role
            )



-- ###########################################
-- View
-- ###########################################


view : Model -> View Msg
view model =
    { title = model.card.label
    , body =
        Theme.container [] <|
            column [ spacing 40, width fill ]
                [ row
                    [ spacingXY 20 0 ]
                    [ el [ width shrink ] <| RoleCard.cardView model.card
                    , displayDescription model.card
                    ]
                , column [ width fill ]
                    [ h2 [] <| text "Gain XP"
                    , displayBehaviours model.card
                    ]
                , row [ spacingXY 20 0, width fill ]
                    [ h2 [ padding 0 ] <| text "XP"
                    , RoleCard.displayXpSlots model.xp model.card
                    ]
                ]
    }



-- Description


displayDescription : RoleCard Msg -> Element Msg
displayDescription role =
    column [ width fill ]
        [ h2 [] <| paragraph [] [ text role.shortDescription ]
        , paragraph [ Font.justify ] [ text role.longDescription ]
        ]



-- Behaviour


displayBehaviours : RoleCard Msg -> Element Msg
displayBehaviours role =
    column
        [ width fill
        , Background.color <| Theme.darken 4 <| RoleCard.colorOf role.level
        ]
        (List.map displayBehaviour role.behaviours)


displayBehaviour : Behaviour -> Element Msg
displayBehaviour behaviour =
    Input.button
        [ Border.solid
        , Border.color Color.Dracula.gray
        , Border.width 1
        , width fill
        , padding 14
        ]
        { onPress = Just GainXp
        , label =
            row [ spacingXY 14 0 ]
                [ paragraph [ width shrink ] [ text "+1" ]
                , paragraph [ Font.justify ] [ text behaviour ]
                ]
        }


-- ###########################################
-- Subscriptions
-- ###########################################


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
