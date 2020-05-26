module Warrior.Npc.StationaryAttacker exposing (takeTurn)

import Warrior.Direction as Direction
import Warrior.Map as Map exposing (Map)
import Warrior.Player as Player exposing (Action(..), Player)


takeTurn : Player -> Map -> Action
takeTurn player map =
    let
        currentPosition =
            Player.currentPosition player

        canAttack direction =
            Map.look direction currentPosition map == Map.Player
    in
    Direction.all
        |> List.filter canAttack
        |> List.head
        |> Maybe.map Attack
        |> Maybe.withDefault Wait
