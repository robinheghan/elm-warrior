module Warrior.Npc.StationaryAttacker exposing (takeTurn)

{-| AI of a villain which attacks anything that gets close.

@docs takeTurn

-}

import Warrior.Direction as Direction
import Warrior.Map as Map exposing (Map)
import Warrior.Player as Player exposing (Action(..), Player)


{-| Use this function with the `withNPC` function of the `Map` module to add dangerous opponents to a map.
-}
takeTurn : Player -> Map -> Action
takeTurn player map =
    let
        currentPosition =
            Player.position player

        canAttack direction =
            Map.look direction currentPosition map
                |> List.head
                |> Maybe.map ((==) Map.Player)
                |> Maybe.withDefault False
    in
    Direction.all
        |> List.filter canAttack
        |> List.head
        |> Maybe.map Attack
        |> Maybe.withDefault Wait
