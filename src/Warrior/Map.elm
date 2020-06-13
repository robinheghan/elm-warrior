module Warrior.Map exposing
    ( Map
    , look, lookDown
    )

{-| The functions in this module allows you to create your own maps, or simply ask questions about the map currently being played.

@docs Map


# Search

@docs look, lookDown

-}

import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction exposing (Direction)
import Warrior.Internal.Map as Internal exposing (Map)
import Warrior.Map.Tile exposing (Tile)
import Warrior.Player exposing (Player)


{-| A map, or level.
-}
type alias Map =
    Internal.Map


{-| Provides a list of everything the player can see in a specific direction. The first item of the list will be the one tile away from the player. The second item will be two tiles away, etc.
-}
look : Direction -> Player -> Map -> List ( Coordinate, Tile )
look =
    Internal.look


{-| Describe what is at the players feet. Useful for deciding if there's an item worth picking up.
-}
lookDown : Player -> Map -> Tile
lookDown =
    Internal.lookDown
