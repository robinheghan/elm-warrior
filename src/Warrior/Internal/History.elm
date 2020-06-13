module Warrior.Internal.History exposing
    ( History
    , init
    , previousActions
    , previousStates
    , record
    , roundsPlayed
    )

import Warrior.Map exposing (Map)
import Warrior.Player as Player exposing (Player)


type History
    = History (List ( Player, Map, Player.Action ))


init : History
init =
    History []


record : Player -> Map -> Player.Action -> History -> History
record player map action (History history) =
    History <| ( player, map, action ) :: history


previousStates : Player -> History -> List ( Player, Map )
previousStates forPlayer (History history) =
    history
        |> List.filter (playerFilter forPlayer)
        |> List.map (\( player, map, _ ) -> ( player, map ))


previousActions : Player -> History -> List Player.Action
previousActions player (History history) =
    history
        |> List.filter (playerFilter player)
        |> List.map (\( _, _, action ) -> action)


playerFilter : Player -> ( Player, Map, Player.Action ) -> Bool
playerFilter player ( pastPlayer, _, _ ) =
    Player.id player == Player.id pastPlayer


roundsPlayed : History -> Int
roundsPlayed (History events) =
    let
        maybeFirstPlayer =
            events
                |> List.reverse
                |> List.head
                |> Maybe.map (\( player, _, _ ) -> player)
    in
    case maybeFirstPlayer of
        Nothing ->
            0

        Just firstPlayer ->
            events
                |> List.filter (playerFilter firstPlayer)
                |> List.length
                |> (-) 1
                |> max 0
