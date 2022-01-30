module Pages.Role.Id_ exposing (Model, Msg, page)

import Element exposing (..)
import Element.Input
import Gen.Params.Role.Id_ exposing (Params)
import Gen.Route
import Page
import Request
import Domain.Role
import Domain.RoleCard exposing (Behaviour, RoleCard)
import Shared
import Theme
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ req =
    Page.element
        { init = init req
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- Model


type Msg
    = GainXp


type alias Model =
    { card : RoleCard Msg }



-- Init


init : Request.With Params -> ( Model, Cmd Msg )
init req =
    case Domain.RoleCard.findById req.params.id of
        Just card ->
            ( { card = card }
            , Cmd.none
            )

        Nothing ->
            ( { card = Domain.RoleCard.fromRole Domain.Role.Mobber }
            , Request.pushRoute Gen.Route.NotFound req
            )



-- Update


update : Msg -> model -> ( model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- View


view : Model -> View Msg
view { card } =
    { title = card.label
    , body = Theme.container [ fullView card ]
    }


fullView : RoleCard Msg -> Element Msg
fullView role =
    column
        [ spacingXY 0 10, width fill ]
        [ el [ width (px 50) ] role.icon
        , text role.label
        , column []
            (List.map displayBehaviour role.behaviours)
        ]


displayBehaviour : Behaviour -> Element Msg
displayBehaviour behaviour =
    Element.Input.button []
        { onPress = Just GainXp
        , label = text behaviour
        }



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
