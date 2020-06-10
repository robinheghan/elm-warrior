module Warrior.Tile exposing (..)

import Warrior.Item exposing (Item)


{-| Describes what is on a specific coordinate of a map.
-}
type Tile
    = Wall
    | Empty
    | SpawnPoint
    | Exit
    | Player String
    | Item Item


isWall : Tile -> Bool
isWall tile =
    case tile of
        Wall ->
            True

        _ ->
            False


isEmpty : Tile -> Bool
isEmpty tile =
    case tile of
        Empty ->
            True

        _ ->
            False


isSpawnPoint : Tile -> Bool
isSpawnPoint tile =
    case tile of
        SpawnPoint ->
            True

        _ ->
            False


isExit : Tile -> Bool
isExit tile =
    case tile of
        Exit ->
            True

        _ ->
            False


isPlayer : Tile -> Bool
isPlayer tile =
    case tile of
        Player _ ->
            True

        _ ->
            False


isItem : Tile -> Bool
isItem tile =
    case tile of
        Item _ ->
            True

        _ ->
            False


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
