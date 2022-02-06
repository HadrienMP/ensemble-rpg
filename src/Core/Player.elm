module Core.Player exposing (..)

import AssocList as Dict exposing (Dict)
import Core.Level exposing (..)
import Core.PlayerId as PlayerId exposing (..)
import Core.PlayerName
import Core.Role exposing (Role)
import Core.RoleCard as RoleCard exposing (RoleCard)
import Core.XpProgress exposing (XpProgress, completed)
import Js.DecoderExtra
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Random
import Random.Char


type alias Player =
    { id : PlayerId
    , name : String
    , icon : Char
    , xp : Dict Role Int
    }


unknown : Player
unknown =
    Player PlayerId.empty [ChangedIdentity {icon}]



-- Json


encode : Player -> Json.Encode.Value
encode player =
    Json.Encode.object
        [ ( "id", PlayerId.encode player.id )
        , ( "name", Json.Encode.string player.name )
        , ( "icon", Json.Encode.string (String.fromChar player.icon) )
        , ( "xp"
          , player.xp
                |> Dict.toList
                |> List.map (\( role, x ) -> [ ( "role", RoleCard.encode role ), ( "xp", Json.Encode.int x ) ])
                |> Json.Encode.list Json.Encode.object
          )
        ]


decoder : Decoder Player
decoder =
    Json.Decode.succeed Player
        |> required "id" PlayerId.decoder
        |> required "name" Json.Decode.string
        |> required "icon" Js.DecoderExtra.firstCharDecoder
        |> required "xp" playerXpDecoder


playerXpDecoder : Decoder (Dict Role Int)
playerXpDecoder =
    Json.Decode.succeed Tuple.pair
        |> required "role" RoleCard.decoder
        |> required "xp" Json.Decode.int
        |> Json.Decode.list
        |> Json.Decode.map Dict.fromList



--


generator : Random.Generator Player
generator =
    Random.map3 (\a b c -> Player a b c Dict.empty)
        PlayerId.generator
        Core.PlayerName.generator
        Random.Char.emoticon


progressOf : Role -> Player -> XpProgress
progressOf role player =
    let
        roleCard =
            RoleCard.fromRole role
    in
    { current = Dict.get roleCard.role player.xp |> Maybe.withDefault 0
    , max = roleCard.xpToComplete
    , role = roleCard.role
    }


updateXp : Int -> Role -> Player -> Player
updateXp xp role player =
    { player | xp = Dict.insert role xp player.xp }


gainXp : Role -> Player -> Player
gainXp role player =
    let
        xp =
            RoleCard.incXp (progressOf role player)
    in
    { player | xp = Dict.insert xp.role xp.current player.xp }


withName : String -> Player -> Player
withName name player =
    { player | name = name }


numberOfBadgesWon : Player -> Int
numberOfBadgesWon player =
    List.length <| completedRoleCards player


completedRoleCards : Player -> List (RoleCard msg)
completedRoleCards player =
    player.xp
        |> Dict.toList
        |> List.map (Tuple.mapFirst RoleCard.fromRole)
        |> List.filter (\( role, xp ) -> xp >= role.xpToComplete)
        |> List.map Tuple.first


accessibleLevels : Player -> List Level
accessibleLevels player =
    all
        |> List.filter (isLevelAccessibleTo player)


isLevelAccessibleTo : Player -> Level -> Bool
isLevelAccessibleTo player level =
    case level of
        Level1 ->
            True

        Level2 ->
            countRolesCompleted player Level1 >= 1

        _ ->
            previous level
                |> Maybe.map (countRolesCompleted player)
                |> Maybe.map (\count -> count >= 2)
                |> Maybe.withDefault False


countRolesCompleted : Player -> Level -> Int
countRolesCompleted player level =
    completedRoleCards player
        |> List.map .level
        |> List.filter (\l -> l == level)
        |> List.length


accessibleRoles : Player -> List (RoleCard msg)
accessibleRoles player =
    let
        levels =
            accessibleLevels player
    in
    RoleCard.all
        |> List.filter (\card -> List.member card.level levels)
        |> List.filter (\card -> progressOf card.role player |> not << completed)
