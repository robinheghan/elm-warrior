module Warrior.Npc.StationaryAttacker exposing (takeTurn)

{-| A villain which will attack anything that gets close.

@docs takeTurn

-}

import Warrior exposing (Action(..), Warrior)
import Warrior.Direction as Direction
import Warrior.History exposing (History)
import Warrior.Map as Map exposing (Map)
import Warrior.Map.Tile as Tile


{-| Use this function with the `withNpc` function of the `Warrior.Map.Builder` module to add dangerous opponents to a map.
-}
takeTurn : Warrior -> Map -> History -> Action
takeTurn player map _ =
    let
        canAttack direction =
            Map.look direction player map
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.map Tile.isPlayer
                |> Maybe.withDefault False
    in
    Direction.all
        |> List.filter canAttack
        |> List.head
        |> Maybe.map Attack
        |> Maybe.withDefault Wait
