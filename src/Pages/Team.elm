module Pages.Team exposing (Model, Msg, page)

import AssocList as Dict
import Core.Player exposing (Player)
import Core.Role.Card.RoleCard as RoleCard exposing (DisplayMode(..))
import Element exposing (column, paddingEach, spacing, spacingXY, text)
import Gen.Params.Team exposing (Params)
import Page
import Request
import Shared
import UI.Theme exposing (container, emptySides, h2)
import View exposing (View)
import Element exposing (wrappedRow)


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
                [ column []
                    [ h2 [ paddingEach { emptySides | bottom = 10 } ] <| text model.player.name
                    , wrappedRow [ spacing 10 ]
                        (model.player.xp
                            |> Dict.toList
                            |> List.map (Tuple.mapFirst RoleCard.fromRole)
                            |> List.filter (\( role, xp ) -> xp >= role.xpToComplete)
                            |> List.map Tuple.first
                            |> List.map (RoleCard.view Badge)
                        )
                    ]
                ]
    }
