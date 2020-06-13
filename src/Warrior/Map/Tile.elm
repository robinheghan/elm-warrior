module Warrior.Map.Tile exposing
    ( Tile(..)
    , isWall, isEmpty, isSpawnPoint, isExit, isWarrior, isItem, canMoveOnto
    )

{-| A map is built of several tiles. You can use this module to get a better idea of how the map is made up.

@docs Tile


# Predicates

Sometimes a pattern match is a bit much when all you want is the answer to a simple question. So here are simple predicate functions that make it easy to answer the most basic question about a tile.

@docs isWall, isEmpty, isSpawnPoint, isExit, isWarrior, isItem, canMoveOnto

-}

import Warrior.Item exposing (Item)


{-| Describes what is on a specific coordinate of a map.
-}
type Tile
    = Wall
    | Empty
    | SpawnPoint
    | Exit
    | Warrior String
    | Item Item


{-| True if the given Tile is a Wall
-}
isWall : Tile -> Bool
isWall tile =
    case tile of
        Wall ->
            True

        _ ->
            False


{-| True if the given Tile represents Empty space
-}
isEmpty : Tile -> Bool
isEmpty tile =
    case tile of
        Empty ->
            True

        _ ->
            False


{-| True if the given Tile is a spawn point
-}
isSpawnPoint : Tile -> Bool
isSpawnPoint tile =
    case tile of
        SpawnPoint ->
            True

        _ ->
            False


{-| True if the given Tile is an exit point
-}
isExit : Tile -> Bool
isExit tile =
    case tile of
        Exit ->
            True

        _ ->
            False


{-| True if the given Tile represents another warrior
-}
isWarrior : Tile -> Bool
isWarrior tile =
    case tile of
        Warrior _ ->
            True

        _ ->
            False


{-| True if the given Tile represents an item
-}
isItem : Tile -> Bool
isItem tile =
    case tile of
        Item _ ->
            True

        _ ->
            False


{-| True if a warrior can move to this tile with a move action.
-}
canMoveOnto : Tile -> Bool
canMoveOnto tile =
    case tile of
        Empty ->
            True

        Item _ ->
            True

        Exit ->
            True

        SpawnPoint ->
            True

        _ ->
            False
