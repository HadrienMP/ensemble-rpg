module Pages.Role.Id_ exposing (Model, Msg, page)

import Color.Dracula
import Core.Role
import Core.Role.Card.RoleCard as RoleCard exposing (Behaviour, RoleCard)
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
import UI.Theme as Theme exposing (emptySides, h2)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ req =
    Page.element
        { init = init req
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
    { role : RoleCard Msg }



-- ###########################################
-- Init
-- ###########################################


init : Request.With Params -> ( Model, Cmd Msg )
init req =
    case RoleCard.findById req.params.id of
        Just card ->
            ( { role = card }
            , Cmd.none
            )

        Nothing ->
            ( { role = RoleCard.fromRole Core.Role.Mobber }
            , Request.replaceRoute Gen.Route.NotFound req
            )



-- ###########################################
-- Update
-- ###########################################


update : Msg -> model -> ( model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- ###########################################
-- View
-- ###########################################


view : Model -> View Msg
view { role } =
    { title = role.label
    , body =
        Theme.container <|
            column [ spacing 40 ]
                [ row
                    [ spacingXY 20 0 ]
                    [ el [ width shrink ] <| RoleCard.cardView role
                    , displayDescription role
                    ]
                , column [ width fill ]
                    [ h2 [] <| text "Gain XP"
                    , displayBehaviours role
                    ]
                , row [ spacingXY 20 0, width fill ]
                    [ h2 [ padding 0 ] <| text "XP"
                    , displayXpSlots role
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



-- XP slots


displayXpSlots : RoleCard Msg -> Element Msg
displayXpSlots role =
    el
        [ width fill
        , Border.solid
        , Border.width 1
        , Border.color Color.Dracula.gray
        , padding 6
        , behindContent <| displayXp 1 role.xpToComplete
        , clipY
        ]
    <|
        el [ centerX ] <|
            text <|
                String.fromInt 1
                    ++ "/"
                    ++ (role.xpToComplete |> RoleCard.xpToCompleteAsInt |> String.fromInt)


displayXp : Int -> RoleCard.XpToComplete -> Element Msg
displayXp current max =
    RoleCard.xpToCompleteAsInt max
        |> List.range 1
        |> List.map
            (\xp ->
                if xp <= current then
                    [ Background.color Color.Dracula.green
                    , Border.shadow
                        { offset = ( 0, 0 )
                        , blur = 6
                        , size = 3 
                        , color = Color.Dracula.green
                        }
                    ]

                else
                    []
            )
        |> List.map (\attr -> el (attr ++ [ width fill, height (px 25) ]) none)
        |> row [ width fill ]



-- ###########################################
-- Subscriptions
-- ###########################################


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
