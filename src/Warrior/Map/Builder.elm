module Warrior.Map.Builder exposing
    ( Template
    , Size, init, withDescription, withSpawnPoint, withExitPoint, withWalledArea
    , withNpc, armLastNpc, withItem
    , spawnPoints, npcs
    , build
    )

{-| You can use this module to build your own maps!

@docs Template


# Map layout

@docs Size, init, withDescription, withSpawnPoint, withExitPoint, withWalledArea


# Non-playable characters (Npc's) and Items

@docs withNpc, armLastNpc, withItem


# Queries

@docs spawnPoints, npcs


# Finally

@docs build

-}

import Array exposing (Array)
import Warrior exposing (Warrior)
import Warrior.Coordinate exposing (Coordinate)
import Warrior.History exposing (History)
import Warrior.Internal.Map as Map exposing (Map)
import Warrior.Internal.Warrior as Player
import Warrior.Item exposing (Item)
import Warrior.Map.Tile as Tile exposing (Tile)


{-| A map in progress.
-}
type Template
    = Template Internals


type alias Internals =
    { description : String
    , tilesPerRow : Int
    , tiles : Array Tile
    , items : List ( Coordinate, Item )
    , npcs : List ( Warrior, Warrior -> Map -> History -> Warrior.Action )
    }


{-| Describes how large a map should be in rows and columns.
-}
type alias Size =
    { rows : Int
    , columns : Int
    }


{-| Initialize an empty map of a given size where every tile is empty. Use the the following `with` functions to make the map more interesting.
-}
init : Size -> Template
init config =
    Template
        { description = ""
        , tilesPerRow = config.columns
        , tiles = Array.initialize (config.columns * config.rows) (always Tile.Empty)
        , items = []
        , npcs = []
        }


{-| Sets a description for the map which will be displayed above the map when the game is played.
-}
withDescription : String -> Template -> Template
withDescription description (Template fields) =
    Template { fields | description = description }


{-| Marks a coordinate on the map where a playable warrior will spawn.
-}
withSpawnPoint : Coordinate -> Template -> Template
withSpawnPoint cord map =
    if coordinatesInBound map cord then
        updateTiles (Array.set (translateCoordinates map cord) Tile.SpawnPoint) map

    else
        map


{-| Marks a coordinate on the map where the player needs to go to advance to the next map.
-}
withExitPoint : Coordinate -> Template -> Template
withExitPoint cord map =
    if coordinatesInBound map cord then
        updateTiles (Array.set (translateCoordinates map cord) Tile.Exit) map

    else
        map


{-| Turns every tile between two coordinates into wall tiles.
-}
withWalledArea : Coordinate -> Coordinate -> Template -> Template
withWalledArea cord1 cord2 ((Template fields) as map) =
    let
        origin =
            { x = min cord1.x cord2.x
            , y = min cord1.y cord2.y
            }

        tilesToFill =
            diff cord1.x cord2.x

        linesToAdjust =
            diff cord1.y cord2.y

        diff a b =
            [ a, b ]
                |> List.sort
                |> List.foldl (-) 0

        updatedTiles =
            List.range 0 linesToAdjust
                |> List.map (\offset -> { origin | y = origin.y + offset })
                |> List.concatMap tileCoordinates
                |> List.filter (coordinatesInBound map)
                |> List.map (translateCoordinates map)
                |> List.foldl (\pos mapFields -> Array.set pos Tile.Wall mapFields) fields.tiles

        tileCoordinates cord =
            List.range 0 tilesToFill
                |> List.map (\offset -> { cord | x = origin.x + offset })
    in
    Template { fields | tiles = updatedTiles }


{-| Places a villain on the specific coordinate of the map, using the supplied function to know what to do each turn. You can find pre-made turn functions in the `Warrior.Npc` module.
-}
withNpc : String -> Coordinate -> (Warrior -> Map -> History -> Warrior.Action) -> Template -> Template
withNpc id cord turnFunc ((Template fields) as map) =
    if coordinatesInBound map cord then
        let
            cleansedNpcs =
                List.filter (\( npc, _ ) -> Warrior.position npc /= cord) fields.npcs
        in
        Template { fields | npcs = ( Player.spawnVillain id cord, turnFunc ) :: cleansedNpcs }

    else
        map


{-| Places an item into the inventory of the last villain added with the `withNpc` function.
-}
armLastNpc : Item -> Template -> Template
armLastNpc item ((Template fields) as builder) =
    case fields.npcs of
        [] ->
            builder

        ( lastNpcState, lastNpcBrain ) :: rest ->
            Template { fields | npcs = ( Player.addItem item lastNpcState, lastNpcBrain ) :: rest }


{-| Places an item on the map which can be picked up by warriors.
-}
withItem : Coordinate -> Item -> Template -> Template
withItem cord item ((Template fields) as map) =
    if coordinatesInBound map cord then
        let
            cleansedItems =
                List.filter (\( itemCord, _ ) -> itemCord /= cord) fields.items
        in
        Template { fields | items = ( cord, item ) :: cleansedItems }

    else
        map


{-| A list of points where warriors can spawn.
-}
spawnPoints : Template -> List Coordinate
spawnPoints (Template fields) =
    Array.indexedMap Tuple.pair fields.tiles
        |> Array.filter (\( _, tile ) -> tile == Tile.SpawnPoint)
        |> Array.map (Tuple.first >> indexToCoordinate fields)
        |> Array.toList


{-| Return a list of all non-playable characters along with their turn functions.
-}
npcs : Template -> List ( Warrior, Warrior -> Map -> History -> Warrior.Action )
npcs (Template fields) =
    fields.npcs


{-| Turn this template into a map
-}
build : Template -> Map
build (Template fields) =
    Map.Map
        { description = fields.description
        , tilesPerRow = fields.tilesPerRow
        , tiles = fields.tiles
        , items = fields.items
        , npcs = List.map Tuple.first fields.npcs
        }



-- HELPERS


coordinatesInBound : Template -> Coordinate -> Bool
coordinatesInBound (Template fields) cord =
    let
        totalRows =
            Array.length fields.tiles // fields.tilesPerRow
    in
    (cord.y >= 0)
        && (cord.y < totalRows)
        && (cord.x >= 0)
        && (cord.x < fields.tilesPerRow)


translateCoordinates : Template -> Coordinate -> Int
translateCoordinates (Template fields) cord =
    let
        colBase =
            cord.y * fields.tilesPerRow
    in
    colBase + cord.x


indexToCoordinate : Internals -> Int -> Coordinate
indexToCoordinate fields index =
    { x = remainderBy fields.tilesPerRow index
    , y = index // fields.tilesPerRow
    }


updateTiles : (Array Tile -> Array Tile) -> Template -> Template
updateTiles fn (Template fields) =
    Template { fields | tiles = fn fields.tiles }
