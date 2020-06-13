module Warrior.Internal.History exposing
    ( History
    , init
    , previousActions
    , previousStates
    , record
    , roundsPlayed
    )

import Warrior exposing (Warrior)
import Warrior.Map exposing (Map)


type History
    = History (List ( Warrior, Map, Warrior.Action ))


init : History
init =
    History []


record : Warrior -> Map -> Warrior.Action -> History -> History
record player map action (History history) =
    History <| ( player, map, action ) :: history


previousStates : Warrior -> History -> List ( Warrior, Map )
previousStates forWarrior (History history) =
    history
        |> List.filter (playerFilter forWarrior)
        |> List.map (\( player, map, _ ) -> ( player, map ))


previousActions : Warrior -> History -> List Warrior.Action
previousActions player (History history) =
    history
        |> List.filter (playerFilter player)
        |> List.map (\( _, _, action ) -> action)


playerFilter : Warrior -> ( Warrior, Map, Warrior.Action ) -> Bool
playerFilter player ( pastWarrior, _, _ ) =
    Warrior.id player == Warrior.id pastWarrior


roundsPlayed : History -> Int
roundsPlayed (History events) =
    let
        maybeFirstWarrior =
            events
                |> List.reverse
                |> List.head
                |> Maybe.map (\( player, _, _ ) -> player)
    in
    case maybeFirstWarrior of
        Nothing ->
            0

        Just firstWarrior ->
            events
                |> List.filter (playerFilter firstWarrior)
                |> List.length
                |> (-) 1
                |> max 0
