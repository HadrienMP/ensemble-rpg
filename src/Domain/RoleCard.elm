module Domain.RoleCard exposing (..)

import AssocList exposing (Dict)
import AssocList.Extra
import BadgeIcon exposing (..)
import Element exposing (..)
import Gen.Route exposing (Route(..))
import Domain.Level exposing (..)
import List
import Domain.Role exposing (..)



-- Types


type XpToComplete
    = Three
    | Four


type alias Behaviour
    = String

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
    }



-- Display



-- List


all : List (RoleCard msg)
all =
    Domain.Role.all
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
            , icon = BadgeIcon.driver
            , behaviours = []
            }

        Navigator ->
            { role = Navigator
            , id = "navigator"
            , label = "Navigator"
            , xpToComplete = Three
            , level = Level1
            , icon = BadgeIcon.navigator
            , behaviours = []
            }

        Mobber ->
            { role = Mobber
            , id = "mobber"
            , label = "Mobber"
            , xpToComplete = Three
            , level = Level1
            , icon = BadgeIcon.mobber
            , behaviours = []
            }

        Researcher ->
            { role = Researcher
            , id = "researcher"
            , label = "Researcher"
            , xpToComplete = Three
            , level = Level2
            , icon = BadgeIcon.researcher
            , behaviours = []
            }

        Sponsor ->
            { role = Sponsor
            , id = "sponsor"
            , label = "Sponsor"
            , xpToComplete = Three
            , level = Level2
            , icon = BadgeIcon.sponsor
            , behaviours = []
            }

        RearAdmiral ->
            { role = RearAdmiral
            , id = "rear-admiral"
            , label = "Rear Admiral"
            , xpToComplete = Three
            , level = Level2
            , icon = BadgeIcon.rearAdmiral
            , behaviours = []
            }

        Automationist ->
            { role = Automationist
            , id = "automationist"
            , label = "Automationist"
            , xpToComplete = Three
            , level = Level3
            , icon = BadgeIcon.automationist
            , behaviours = []
            }

        Nose ->
            { role = Nose
            , id = "nose"
            , label = "Nose"
            , xpToComplete = Three
            , level = Level3
            , icon = BadgeIcon.nose
            , behaviours = []
            }

        Archivist ->
            { role = Archivist
            , id = "archivist"
            , label = "Archivist"
            , xpToComplete = Three
            , level = Level3
            , icon = BadgeIcon.archivist
            , behaviours = []
            }

        TrafficCop ->
            { role = TrafficCop
            , id = "traffic-cop"
            , label = "Traffic Cop"
            , xpToComplete = Three
            , level = Level4
            , icon = BadgeIcon.trafficCop
            , behaviours = []
            }



-- Find


findById : RoleId -> Maybe (RoleCard msg)
findById id =
    all
        |> List.filter (\c -> c.id == id)
        |> List.head
