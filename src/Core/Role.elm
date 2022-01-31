module Core.Role exposing (..)


type Role
    = Driver
    | Navigator
    | Mobber
    | Researcher
    | Sponsor
    | RearAdmiral
    | Automationist
    | Nose
    | Archivist
    | TrafficCop


all : List Role
all =
    next [] |> List.reverse


next : List Role -> List Role
next list =
    case List.head list of
        Nothing ->
            Driver :: list |> next

        Just Driver ->
            Navigator :: list |> next

        Just Navigator ->
            Mobber :: list |> next

        Just Mobber ->
            Researcher :: list |> next

        Just Researcher ->
            Sponsor :: list |> next

        Just Sponsor ->
            RearAdmiral :: list |> next

        Just RearAdmiral ->
            Automationist :: list |> next

        Just Automationist ->
            Nose :: list |> next

        Just Nose ->
            Archivist :: list |> next

        Just Archivist ->
            TrafficCop :: list |> next

        Just TrafficCop ->
            list
