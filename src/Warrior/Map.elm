module Warrior.Map exposing
    ( Map
    , canMoveOnto, canMoveOntoTile, isExitPoint, look, lookDown
    )

{-| The functions in this module allows you to create your own maps, or simply ask questions about the map currently being played.

@docs Map, Config, Tile


# Player API

@docs canMoveOnto, canMoveOntoTile, isExitPoint, look, lookDown

-}

import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction exposing (Direction)
import Warrior.Internal.Map as Internal exposing (Map)
import Warrior.Player exposing (Player)
import Warrior.Tile exposing (Tile)


{-| A map, or level.
-}
type alias Map =
    Internal.Map


{-| Checks to see if a specific coordinate on the map is an exit point.
-}
isExitPoint : Map -> Coordinate -> Bool
isExitPoint =
    Internal.isExitPoint


{-| Provides a list of everything the player can see in a specific direction. The first item of the list will be the one tile away from the player. The second item will be two tiles away, etc.
-}
look : Direction -> Coordinate -> Map -> List Tile
look =
    Internal.look


{-| Describe what is at the players feet. Useful for deciding if there's an item worth picking up.
-}
lookDown : Player -> Map -> Tile
lookDown =
    Internal.lookDown


{-| Checks if a Move action can be performed.
-}
canMoveOnto : Coordinate -> Map -> Bool
canMoveOnto =
    Internal.canMoveOnto


{-| Checks if a Move action can be formed onto the given tile.
-}
canMoveOntoTile : Tile -> Bool
canMoveOntoTile =
    Internal.canMoveOntoTile
