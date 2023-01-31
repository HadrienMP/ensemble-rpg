module Pages.Entry exposing (Model, Msg, page)

import Browser.Events
import Color.Dracula
import Core.Room exposing (Room)
import Effect exposing (Effect)
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Element.Region
import Gen.Params.Entry exposing (Params)
import Gen.Route
import Json.Decode
import Page
import Random
import Request
import Shared
import UI.Theme
import Uuid
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { name : String, uuid : Maybe Uuid.Uuid }


init : ( Model, Effect Msg )
init =
    ( { name = "", uuid = Nothing }, Effect.fromCmd <| Random.generate GotUuid Uuid.uuidGenerator )



-- UPDATE


type Msg
    = SelectedRoom
    | Ignore
    | NameChanged String
    | GotUuid Uuid.Uuid


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SelectedRoom ->
            case model.uuid of
                Just uuid ->
                    ( model, Effect.fromShared (Shared.SelectRoom uuid <| Core.Room.fromString model.name) )

                Nothing ->
                    ( model, Effect.none )

        NameChanged name ->
            ( { model | name = name }, Effect.none )

        Ignore ->
            ( model, Effect.none )

        GotUuid uuid ->
            ( { model | uuid = Just uuid }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress
        (keyDecoder
            |> Json.Decode.map
                (\key ->
                    if key == "Enter" then
                        SelectedRoom

                    else
                        Ignore
                )
        )


keyDecoder : Json.Decode.Decoder String
keyDecoder =
    Json.Decode.field "key" Json.Decode.string



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Homepage"
    , body =
        Element.column
            [ Element.centerX
            , Element.centerY
            , Element.spacing 20
            ]
            [ Element.el
                [ Element.Region.heading 1
                , Element.Font.size 30
                , Element.Font.color Color.Dracula.green
                ]
              <|
                Element.text "Ensemble RPG"
            , Element.Input.text [ Element.Background.color Color.Dracula.black ]
                { onChange = NameChanged
                , text = model.name
                , label = Element.Input.labelLeft [] <| Element.text "room"
                , placeholder = Just <| Element.Input.placeholder [] <| Element.text "my-cool-room-name"
                }
            , Element.Input.button
                [ Element.width Element.fill
                , Element.Background.color Color.Dracula.green
                , Element.padding 10
                , Element.Border.rounded 4
                , Element.Font.color Color.Dracula.black
                , Element.Font.center
                , Element.Font.bold
                , Element.Font.size 20
                ]
                { onPress = Just SelectedRoom, label = Element.text "Join" }
            ]
    }
