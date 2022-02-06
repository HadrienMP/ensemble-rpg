module Core.RoleCard exposing (..)

import AssocList exposing (Dict)
import AssocList.Extra
import Color.Dracula
import Core.Level exposing (..)
import Core.Role exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Json.Decode
import Json.Encode
import List
import UI.BadgeIcon
import UI.Theme exposing (CardSize(..))



-- Types


type alias XpToComplete =
    Int


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



-- Json


encode : Role -> Json.Encode.Value
encode role =
    fromRole role |> .id |> Json.Encode.string


decoder : Json.Decode.Decoder Role
decoder =
    Json.Decode.string
        |> Json.Decode.map findById
        |> Json.Decode.map (Maybe.map .role)
        |> Json.Decode.map (Maybe.withDefault Driver)



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
            , xpToComplete = 3
            , level = Level1
            , icon = UI.BadgeIcon.driver
            , behaviours =
                [ "Ask a clarifying question about what to type"
                , "Type something you disagree with"
                , "Use a new keyboard shortcut"
                , "Learn something new about tooling"
                , "Ignore a direct instruction from someone who isn't the Navigator"
                ]
            , shortDescription = "The tools expert"
            , longDescription = "The quiet professional, you bring the mob quicky through the cycle : red, green, refactor."
            }

        Navigator ->
            { role = Navigator
            , id = "navigator"
            , label = "Navigator"
            , xpToComplete = 3
            , level = Level1
            , icon = UI.BadgeIcon.navigator
            , behaviours =
                [ "Ask for ideas"
                , "filter the mob's idea then tell the driver exactly what to type"
                , "tell the driver only you high-level intent and have them implement the details"
                , "create a failing test, make it pass then refactor"
                ]
            , shortDescription = "Brick by brick, you build in the darkness"
            , longDescription = "Every step you take brings you closer, as you sift the wisdow of the mob"
            }

        Mobber ->
            { role = Mobber
            , id = "mobber"
            , label = "Mobber"
            , xpToComplete = 3
            , level = Level1
            , icon = UI.BadgeIcon.mobber
            , behaviours =
                [ "Yield to the less priviledged voice"
                , "Contribute an idea"
                , "Ask questions until you understand"
                , "Listen on the edge of your seat"
                ]
            , shortDescription = "Shoulder to shoulder with the best"
            , longDescription = "Your relaxed manner belies what you know to be true: nothing can stop this mob from shipping"
            }

        Researcher ->
            { role = Researcher
            , id = "researcher"
            , label = "Researcher"
            , xpToComplete = 4
            , level = Level2
            , icon = UI.BadgeIcon.researcher
            , behaviours =
                [ "Find and share relevant information from documentation/blog/forum etc."
                ]
            , shortDescription = "You track the footsteps of thos who have gone before"
            , longDescription = "Wandering the ailses of an infinite digital library"
            }

        Sponsor ->
            { role = Sponsor
            , id = "sponsor"
            , label = "Sponsor"
            , xpToComplete = 4
            , level = Level2
            , icon = UI.BadgeIcon.sponsor
            , behaviours =
                [ "Amplify the unheard voice"
                , "Pick the mobber with the least priviledge and support their contributions"
                , "Celebrate moments of excellence"
                ]
            , shortDescription = "Always seek the diamond in the rough"
            , longDescription = "Knowing that innovation by definition comes from our blindspots, you fan the flames of unlooked-for greatness in others"
            }

        RearAdmiral ->
            { role = RearAdmiral
            , id = "rear-admiral"
            , label = "Rear Admiral"
            , xpToComplete = 4
            , level = Level2
            , icon = UI.BadgeIcon.rearAdmiral
            , behaviours =
                [ "Quietly speak into the navigators ear"
                , "Give the smallest cue necessary to move the navigator forward"
                , "Navigate the navigator at the highest level of abstraction they can understand"
                ]
            , shortDescription = "At the gates of teeth, you will meet terrible dangers. Here is how you will slip by them"
            , longDescription = "From your darkened corner, you whisper your battle-hardened wisdom, gently advising the captain of this merry band, as they grasp their own chance for glory"
            }

        Automationist ->
            { role = Automationist
            , id = "automationist"
            , label = "Automationist"
            , xpToComplete = 4
            , level = Level3
            , icon = UI.BadgeIcon.automationist
            , behaviours =
                [ "Point out a repeated task in a tool"
                , "Point our a repeated aspect of team process"
                , "Point out possible boiler plate code"
                , "Propose an automation for a repeated task"
                ]
            , shortDescription = "Witness the power of my android army"
            , longDescription = "Relentlessly expanding the power of what a single mind can do, you create countless robot hands that make drudgery a mere memory"
            }

        Nose ->
            { role = Nose
            , id = "nose"
            , label = "Nose"
            , xpToComplete = 4
            , level = Level3
            , icon = UI.BadgeIcon.nose
            , behaviours =
                [ "Point out a long line of code"
                , "Point out a complex conditional"
                , "Point out duplication"
                , "Point out an unnamed variable or method"
                , "Propose an action for deodorizing the code"
                ]
            , shortDescription = "Something's rotten in Denmark"
            , longDescription = "With the sensibilities of a technical sommelier, you sniff out the smelly and malodorous elements in the code. Nothing escapes the Nose!"
            }

        Archivist ->
            { role = Archivist
            , id = "archivist"
            , label = "Archivist"
            , xpToComplete = 4
            , level = Level3
            , icon = UI.BadgeIcon.archivist
            , behaviours =
                [ "Record solution alternatives in a big whiteboard"
                , "Express an idea as it is taking shape in a big whiteboard"
                , "Articulate the current task at hand and make it visible to the entire mob"
                ]
            , shortDescription = "Whitout writing, who will remember us?"
            , longDescription = "In constant battle with the winds of time that erase and obscure, you write, illustrate, and illuminate the great deeds and discoveries of your hardy band."
            }

        TrafficCop ->
            { role = TrafficCop
            , id = "traffic-cop"
            , label = "Traffic Cop"
            , xpToComplete = 4
            , level = Level4
            , icon = UI.BadgeIcon.trafficCop
            , behaviours =
                [ "Suggest a relevant new process or working agreement"
                , "Point out when the team violates a process or working agreement"
                , "Articulate the current task at hand an make it visible to the entire mob"
                , "Capture design decisions and other technical details for the team"
                ]
            , shortDescription = "Follow the rules of the road. It will save your life"
            , longDescription = "You know how even the great can make foolish mistakes when fatigued. So your experienced eyes are always on how we get there not where we're going"
            }



-- Find


findById : RoleId -> Maybe (RoleCard msg)
findById id =
    all
        |> List.filter (\c -> c.id == id)
        |> List.head



-- Display


type DisplayMode
    = Badge
    | Card


view : DisplayMode -> RoleCard msg -> Element msg
view mode role =
    case mode of
        Badge ->
            column
                [ Border.rounded 5
                , Border.solid
                , Border.width 2
                , Border.color Color.Dracula.white
                , Background.color <| UI.Theme.darken 4 <| colorOf role.level
                ]
                [ el [ width <| px 50, centerX, paddingXY 5 5 ] role.icon ]

        Card ->
            UI.Theme.card [ width fill ]
                { icon = role.icon
                , color = colorOf role.level
                , size = Big
                , main = text role.label
                , sub = text <| "Level " ++ Core.Level.toString role.level
                }


cardView : RoleCard msg -> Element msg
cardView =
    view Card


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