module Warrior.Map.Test exposing (expectEqualTiles)

import Array
import Expect
import Html.Attributes exposing (rows)
import Result exposing (map)
import Warrior.Coordinate as Coordinate exposing (Coordinate)
import Warrior.Internal.Map as Map exposing (Map(..))
import Warrior.Item as Item
import Warrior.Map.Tile as Tile exposing (Tile)


{-| Test that the map contains all the given tiles.

    expectEqualTiles
        [ [ Tile.SpawnPoint, Tile.Empty, Tile.Empty, tile.Empty, Tile.Exit ]
        ]
        map

-}
expectEqualTiles : List (List Tile) -> Map -> Expect.Expectation
expectEqualTiles rows map =
    Expect.all
        (rows
            |> padRowsToSize (mapSize map) Tile.Wall
            |> List.indexedMap
                (\y col ->
                    List.indexedMap
                        (\x tile ->
                            expectTileAt { x = x, y = y } tile
                        )
                        col
                )
            |> List.foldr (++) []
        )
        map



--- HELPERS


type alias Size =
    { columns : Int
    , rows : Int
    }


mapSize : Map -> Size
mapSize (Map map) =
    { columns = map.tilesPerRow
    , rows = Array.length map.tiles // map.tilesPerRow
    }


padRowsToSize : Size -> Tile -> List (List Tile) -> List (List Tile)
padRowsToSize size default rows =
    let
        needed : Int
        needed =
            size.rows - List.length rows
    in
    List.map (padColumnsToLength size.columns default) rows
        ++ List.repeat needed (List.repeat size.columns default)


padColumnsToLength : Int -> Tile -> List Tile -> List Tile
padColumnsToLength length default columns =
    let
        needed : Int
        needed =
            length - List.length columns
    in
    columns ++ List.repeat needed default


{-| Expect that the tile at the given coordinate is as expected.

    expectTileAt { x = 1, y = 1 } Tile.SpawnPoint map

-}
expectTileAt : Coordinate -> Tile -> Map -> Expect.Expectation
expectTileAt coord expected map =
    let
        actual : Tile
        actual =
            Map.tileAtPosition coord map
    in
    if actual == expected then
        Expect.pass

    else
        Expect.fail ("Expected to find " ++ tileToString expected ++ " at " ++ Coordinate.toString coord ++ " but found " ++ tileToString actual ++ " instead.")


tileToString : Tile -> String
tileToString tile =
    case tile of
        Tile.Wall ->
            "Wall"

        Tile.Empty ->
            "Empty"

        Tile.SpawnPoint ->
            "SpawnPoint"

        Tile.Exit ->
            "Exit"

        Tile.Warrior name ->
            "Warrior (" ++ name ++ ")"

        Tile.Item item ->
            "Item (" ++ Item.toString item ++ ")"
