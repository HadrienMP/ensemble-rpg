module Core.Role.Card.RoleCard exposing (..)

import AssocList exposing (Dict)
import AssocList.Extra
import Color.Dracula
import Core.Level exposing (..)
import Core.Player exposing (Xp)
import Core.Role exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Gen.Route as Route exposing (Route(..))
import List
import UI.BadgeIcon exposing (..)
import UI.Theme exposing (emptySides)



-- Types


type XpToComplete
    = Three
    | Four


xpToCompleteAsInt : XpToComplete -> Int
xpToCompleteAsInt xp =
    case xp of
        Three ->
            3

        Four ->
            4


type alias Behaviour =
    String


type alias RoleId =
    String


type alias RoleCard msg =
    { role : Role
    , id : RoleId
    , label : String
    , xpToComplete : XpToComplete
    , level : Level
    , icon : Element msg
    , behaviours : List Behaviour
    , longDescription : String
    , shortDescription : String
    }



-- List


all : List (RoleCard msg)
all =
    Core.Role.all
        |> List.map fromRole


byLevel : Dict Level (List (RoleCard msg))
byLevel =
    AssocList.Extra.groupBy .level all


fromRole : Role -> RoleCard msg
fromRole role =
    case role of
        Driver ->
            { role = Driver
            , id = "driver"
            , label = "Driver"
            , xpToComplete = Three
            , level = Level1
            , icon = UI.BadgeIcon.driver
            , behaviours =
                [ "Demandez des idées"
                , "Filtrez la sagesse du mob pour dicter exactement quoi taper au pilote"
                , "Donnez uniquement votre intention au pilote en le laissant gérer les détails d'implémentation"
                , "Créez un test rouge, le faites passer et refactorez"
                ]
            , shortDescription = "The tools expert"
            , longDescription = "The quiet professional, you bring the mob quicky through the cycle : red, green, refactor."
            }

        Navigator ->
            { role = Navigator
            , id = "navigator"
            , label = "Navigator"
            , xpToComplete = Three
            , level = Level1
            , icon = UI.BadgeIcon.navigator
            , behaviours = []
            , shortDescription = "Brick by brick, you build in the darkness"
            , longDescription = "Every step you take brings you closer, as you sift the wisdow of the mob"
            }

        Mobber ->
            { role = Mobber
            , id = "mobber"
            , label = "Mobber"
            , xpToComplete = Three
            , level = Level1
            , icon = UI.BadgeIcon.mobber
            , behaviours = []
            , shortDescription = "The tools expert"
            , longDescription = "Un professionnel silencieux, vous amenez le mob rapidement au travers les étapes : red, green et refactor."
            }

        Researcher ->
            { role = Researcher
            , id = "researcher"
            , label = "Researcher"
            , xpToComplete = Three
            , level = Level2
            , icon = UI.BadgeIcon.researcher
            , behaviours = []
            , shortDescription = "The tools expert"
            , longDescription = "Un professionnel silencieux, vous amenez le mob rapidement au travers les étapes : red, green et refactor."
            }

        Sponsor ->
            { role = Sponsor
            , id = "sponsor"
            , label = "Sponsor"
            , xpToComplete = Three
            , level = Level2
            , icon = UI.BadgeIcon.sponsor
            , behaviours = []
            , shortDescription = "The tools expert"
            , longDescription = "Un professionnel silencieux, vous amenez le mob rapidement au travers les étapes : red, green et refactor."
            }

        RearAdmiral ->
            { role = RearAdmiral
            , id = "rear-admiral"
            , label = "Rear Admiral"
            , xpToComplete = Three
            , level = Level2
            , icon = UI.BadgeIcon.rearAdmiral
            , behaviours = []
            , shortDescription = "The tools expert"
            , longDescription = "Un professionnel silencieux, vous amenez le mob rapidement au travers les étapes : red, green et refactor."
            }

        Automationist ->
            { role = Automationist
            , id = "automationist"
            , label = "Automationist"
            , xpToComplete = Three
            , level = Level3
            , icon = UI.BadgeIcon.automationist
            , behaviours = []
            , shortDescription = "The tools expert"
            , longDescription = "Un professionnel silencieux, vous amenez le mob rapidement au travers les étapes : red, green et refactor."
            }

        Nose ->
            { role = Nose
            , id = "nose"
            , label = "Nose"
            , xpToComplete = Three
            , level = Level3
            , icon = UI.BadgeIcon.nose
            , behaviours = []
            , shortDescription = "The tools expert"
            , longDescription = "Un professionnel silencieux, vous amenez le mob rapidement au travers les étapes : red, green et refactor."
            }

        Archivist ->
            { role = Archivist
            , id = "archivist"
            , label = "Archivist"
            , xpToComplete = Three
            , level = Level3
            , icon = UI.BadgeIcon.archivist
            , behaviours = []
            , shortDescription = "The tools expert"
            , longDescription = "Un professionnel silencieux, vous amenez le mob rapidement au travers les étapes : red, green et refactor."
            }

        TrafficCop ->
            { role = TrafficCop
            , id = "traffic-cop"
            , label = "Traffic Cop"
            , xpToComplete = Three
            , level = Level4
            , icon = UI.BadgeIcon.trafficCop
            , behaviours = []
            , shortDescription = "The tools expert"
            , longDescription = "Un professionnel silencieux, vous amenez le mob rapidement au travers les étapes : red, green et refactor."
            }



-- Find


findById : RoleId -> Maybe (RoleCard msg)
findById id =
    all
        |> List.filter (\c -> c.id == id)
        |> List.head



-- Display


cardView : RoleCard msg -> Element msg
cardView role =
    let
        cardWidth =
            110

        cardHeight : Int
        cardHeight =
            toFloat cardWidth
                |> (*) 1.4
                |> round
    in
    Element.link
        [ Border.rounded 5
        , Border.solid
        , Border.width 2
        , Border.color Color.Dracula.white
        , Background.color <| UI.Theme.darken 4 <| colorOf role.level
        , height <| px cardHeight
        , width <| minimum cardWidth fill
        ]
        { url = Route.Role__Id_ { id = role.id } |> Route.toHref
        , label =
            column [ spacingXY 0 5, width fill ]
                [ el [ width <| px 100, centerX, paddingXY 15 0 ] role.icon
                , el
                    [ centerX
                    , Font.bold
                    , Font.size 14
                    , paddingXY 0 5
                    , Font.shadow { offset = ( 2, 2 ), blur = 2, color = Color.Dracula.gray }
                    ]
                    (text role.label)
                , el
                    [ centerX
                    , Font.size 12
                    , Font.shadow { offset = ( 1, 1 ), blur = 2, color = Color.Dracula.gray }
                    ]
                    (text <| "Level " ++ Core.Level.toString role.level)
                ]
        }


colorOf : Level -> Color
colorOf level =
    case level of
        Level1 ->
            Color.Dracula.purple

        Level2 ->
            Color.Dracula.pink

        Level3 ->
            Color.Dracula.orange

        Level4 ->
            Color.Dracula.red
