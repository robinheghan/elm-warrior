module Warrior.Map exposing
    ( Map
    , look, lookDown
    )

{-| This module contains functions which allow you look around a map, based on the location of a warrior.

@docs Map


# Search

@docs look, lookDown

-}

import Warrior exposing (Warrior)
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction exposing (Direction)
import Warrior.Internal.Map as Internal exposing (Map)
import Warrior.Map.Tile exposing (Tile)


{-| A map, represented by a grid of tiles.
-}
type alias Map =
    Internal.Map


{-| Provides a list of everything the warrior can see in a specific direction. The first item of the list will be the one tile away. The second item will be two tiles away, etc.
-}
look : Direction -> Warrior -> Map -> List ( Coordinate, Tile )
look =
    Internal.look


{-| Describe what is at the warrior's feet. Usually this will just be empty space, but occasionally you might see an item worth picking up.
-}
lookDown : Warrior -> Map -> Tile
lookDown =
    Internal.lookDown
