module Warrior.Map.Progression exposing
    ( Progression(..)
    , ProgressionFunction
    , reachExitPoint
    , reachExitPointWithRoundLimit
    )

import Warrior.History as History exposing (History)
import Warrior.Internal.Player as Player exposing (Player)
import Warrior.Map as Map exposing (Map)
import Warrior.Tile as Tile


type alias ProgressionFunction =
    List Player -> Map -> History -> Progression


type Progression
    = Advance (List Player)
    | GameOver
    | Undecided


reachExitPoint : ProgressionFunction
reachExitPoint players map _ =
    let
        anyoneReachedExit =
            List.any reachedExit players

        reachedExit player =
            Map.lookDown player map
                |> Tile.isExit

        allDead =
            players
                |> List.filter Player.alive
                |> List.isEmpty
    in
    if anyoneReachedExit then
        Advance players

    else if allDead then
        GameOver

    else
        Undecided


reachExitPointWithRoundLimit : Int -> ProgressionFunction
reachExitPointWithRoundLimit roundLimit players map history =
    case reachExitPoint players map history of
        Undecided ->
            if History.roundsPlayed history >= roundLimit then
                GameOver

            else
                Undecided

        otherwise ->
            otherwise
