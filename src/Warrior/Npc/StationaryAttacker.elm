module Warrior.Npc.StationaryAttacker exposing (takeTurn)

{-| AI of a villain which attacks anything that gets close.

@docs takeTurn

-}

import Warrior.Direction as Direction
import Warrior.History exposing (History)
import Warrior.Map as Map exposing (Map)
import Warrior.Player as Player exposing (Action(..), Player)
import Warrior.Tile as Tile


{-| Use this function with the `withNPC` function of the `Map` module to add dangerous opponents to a map.
-}
takeTurn : Player -> Map -> History -> Action
takeTurn player map _ =
    let
        currentPosition =
            Player.position player

        canAttack direction =
            Map.look direction currentPosition map
                |> List.head
                |> Maybe.map Tile.isPlayer
                |> Maybe.withDefault False
    in
    Direction.all
        |> List.filter canAttack
        |> List.head
        |> Maybe.map Attack
        |> Maybe.withDefault Wait
