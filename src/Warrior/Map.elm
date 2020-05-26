module Warrior.Map exposing
    ( Map, Config, Tile(..)
    , canMoveOnto, canMoveOntoTile, isExitPoint, look
    , init, withExitPoint, withItem, withNPC, withSpawnPoint, withWalledArea, armLastNpc, setNpcs
    , coordinateFrom, npcs, removeItem, spawnPoints, view
    )

{-| The functions in this module allows you to create your own maps, or simply ask questions about the map currently being played.

@docs Map, Config, Tile


# Player API

@docs canMoveOnto, canMoveOntoTile, isExitPoint, look


# Creation API

@docs init, withExitPoint, withItem, withNPC, withSpawnPoint, withWalledArea, armLastNpc, setNpcs


# Internals

@docs coordinateFrom, npcs, removeItem, spawnPoints, view

-}

import Array exposing (Array)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.Item exposing (Item)
import Warrior.Player as Player exposing (Player)


{-| A map, or level.
-}
type Map
    = Map Internals


type alias Internals =
    { tilesPerRow : Int
    , tiles : Array Tile
    , items : List ( Coordinate, Item )
    , npcs : List ( Player, Player -> Map -> Player.Action )
    }


{-| Describes what is on a specific coordinate of a map.
-}
type Tile
    = Wall
    | Empty
    | SpawnPoint
    | Player
    | Item Item
    | Exit


{-| Describes how large a map should be in rows and columns. Use this when creating your own maps.
-}
type alias Config =
    { rows : Int
    , columns : Int
    }


{-| Initialize an empty map of a given size where every tile is empty. Use the the following `with` functions to make the map more interesting.
-}
init : Config -> Map
init config =
    Map
        { tilesPerRow = config.columns
        , tiles = Array.initialize (config.columns * config.rows) (always Empty)
        , items = []
        , npcs = []
        }


{-| Marks a coordinate on the map where the player will spawn.
-}
withSpawnPoint : Coordinate -> Map -> Map
withSpawnPoint cord map =
    if coordinatesInBound cord map then
        updateTiles (Array.set (translateCoordinates cord map) SpawnPoint) map

    else
        map


{-| Marks a coordinate on the map where the player needs to go to advance to the next map.
-}
withExitPoint : Coordinate -> Map -> Map
withExitPoint cord map =
    if coordinatesInBound cord map then
        updateTiles (Array.set (translateCoordinates cord map) Exit) map

    else
        map


{-| Turns every tile between two coordinates into walls.
-}
withWalledArea : Coordinate -> Coordinate -> Map -> Map
withWalledArea cord1 cord2 ((Map fields) as map) =
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
                |> List.map (\cord -> translateCoordinates cord map)
                |> List.foldl (\pos mapFields -> Array.set pos Wall mapFields) fields.tiles

        tileCoordinates cord =
            List.range 0 tilesToFill
                |> List.map (\offset -> { cord | x = origin.x + offset })
    in
    Map { fields | tiles = updatedTiles }


{-| Places a villain on the specific coordinate of the map, using the supplied function to know what to do each turn. You can find pre-made turn functions in the `Warrior.Npc` module.
-}
withNPC : Coordinate -> (Player -> Map -> Player.Action) -> Map -> Map
withNPC cord turnFunc (Map fields) =
    Map { fields | npcs = ( Player.spawnVillain cord, turnFunc ) :: fields.npcs }


{-| Replace all villains with the provided list. Might be less code than using multiple `withNPC` calls.
-}
setNpcs : List ( Player, Player -> Map -> Player.Action ) -> Map -> Map
setNpcs newNpcs (Map fields) =
    Map { fields | npcs = newNpcs }


{-| Places an item into the inventory of the last villain added with the `withNPC` function.
-}
armLastNpc : Item -> Map -> Map
armLastNpc item (Map fields) =
    case fields.npcs of
        [] ->
            Map fields

        ( lastNpcState, lastNpcBrain ) :: rest ->
            Map { fields | npcs = ( Player.addItem item lastNpcState, lastNpcBrain ) :: rest }


{-| Places an item on the map which can be picked up by players.
-}
withItem : Coordinate -> Item -> Map -> Map
withItem coordinate item (Map fields) =
    let
        cleansedItems =
            List.filter (\( itemCord, _ ) -> itemCord /= coordinate) fields.items
    in
    Map { fields | items = ( coordinate, item ) :: cleansedItems }


{-| Removes an item from the map. This function can be ignored as it will be called by the framework.
-}
removeItem : Coordinate -> Map -> Maybe ( Item, Map )
removeItem cord (Map fields) =
    let
        maybeItem =
            fields.items
                |> List.filter (\( itemCord, _ ) -> itemCord == cord)
                |> List.head
                |> Maybe.map Tuple.second
    in
    case maybeItem of
        Just item ->
            Just
                ( item
                , Map { fields | items = List.filter (\( itemCord, _ ) -> itemCord /= cord) fields.items }
                )

        Nothing ->
            Nothing


coordinatesInBound : Coordinate -> Map -> Bool
coordinatesInBound cord (Map fields) =
    let
        totalRows =
            Array.length fields.tiles // fields.tilesPerRow
    in
    (cord.y >= 0)
        && (cord.y < totalRows)
        && (cord.x >= 0)
        && (cord.x < fields.tilesPerRow)


translateCoordinates : Coordinate -> Map -> Int
translateCoordinates cord (Map fields) =
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


updateTiles : (Array Tile -> Array Tile) -> Map -> Map
updateTiles fn (Map fields) =
    Map { fields | tiles = fn fields.tiles }


{-| A list of all points where players can spawn.
-}
spawnPoints : Map -> List Coordinate
spawnPoints (Map fields) =
    Array.indexedMap Tuple.pair fields.tiles
        |> Array.filter (\( _, tile ) -> tile == SpawnPoint)
        |> Array.map (Tuple.first >> indexToCoordinate fields)
        |> Array.toList


{-| Framework function
-}
npcs : Map -> List ( Player, Player -> Map -> Player.Action )
npcs (Map fields) =
    fields.npcs


{-| Checks to see if a specific coordinate on the map is an exit point.
-}
isExitPoint : Map -> Coordinate -> Bool
isExitPoint ((Map fields) as map) cord =
    Array.get (translateCoordinates cord map) fields.tiles
        |> Maybe.map ((==) Exit)
        |> Maybe.withDefault False


{-| Framework function
-}
view : List Coordinate -> Map -> Element a
view playerPositions ((Map fields) as map) =
    let
        playerIndices =
            List.map (\cord -> translateCoordinates cord map) playerPositions

        firstAndLast =
            Array.initialize (fields.tilesPerRow + 2) (always Wall)
                |> Array.map (viewTile [] 0)
                |> Array.toList
                |> Element.row []
    in
    fields.tiles
        |> Array.indexedMap (mapTile fields)
        |> Array.indexedMap (viewTile playerIndices)
        |> asRows fields.tilesPerRow []
        |> surroundElements firstAndLast
        |> Element.column []


mapTile : Internals -> Int -> Tile -> Tile
mapTile fields index originalTile =
    if originalTile /= Empty then
        originalTile

    else
        let
            coordinate =
                indexToCoordinate fields index
        in
        tileAtPosition coordinate fields


viewTile : List Int -> Int -> Tile -> Element a
viewTile playerPositionIndices index tile =
    Element.el
        [ Element.width <| Element.px 50
        , Element.height <| Element.px 50
        , Background.color <| tileColor tile
        , Border.width 1
        ]
        (if List.any ((==) index) playerPositionIndices then
            Element.el
                [ Element.centerX
                , Element.centerY
                , Font.color <| Element.rgb255 122 122 122
                ]
                (Element.text "@")

         else
            Element.none
        )


asRows : Int -> List (Element a) -> Array (Element a) -> List (Element a)
asRows tilesPerRow rows remainingTiles =
    if Array.isEmpty remainingTiles then
        List.reverse rows

    else
        let
            nextTiles =
                Array.slice 0 tilesPerRow remainingTiles
                    |> Array.toList
                    |> surroundElements (viewTile [] 0 Wall)
                    |> Element.row []

            tilesAfterSlice =
                Array.slice tilesPerRow (Array.length remainingTiles) remainingTiles
        in
        asRows tilesPerRow (nextTiles :: rows) tilesAfterSlice


surroundElements : Element a -> List (Element a) -> List (Element a)
surroundElements with elements =
    (with :: elements) ++ [ with ]


tileColor : Tile -> Element.Color
tileColor tile =
    case tile of
        Wall ->
            Element.rgb255 0 0 0

        Empty ->
            Element.rgb255 255 255 255

        Player ->
            Element.rgb255 255 255 255

        Item _ ->
            Element.rgba255 255 255 0 0.4

        SpawnPoint ->
            Element.rgb255 0 0 255

        Exit ->
            Element.rgb255 0 255 0



-- Player API


{-| Provides a list of everything the player can see in a specific direction. The first item of the list will be the one tile away from the player. The second item will be two tiles away, etc.
-}
look : Direction -> Coordinate -> Map -> List Tile
look dir from map =
    lookHelp dir from map []


lookHelp : Direction -> Coordinate -> Map -> List Tile -> List Tile
lookHelp dir from ((Map fields) as map) result =
    let
        wantedCoordinate =
            coordinateFrom dir from
    in
    if not (coordinatesInBound wantedCoordinate map) then
        Wall
            :: result
            |> List.reverse

    else
        let
            tile =
                wantedCoordinate
                    |> (\c -> translateCoordinates c map)
                    |> (\idx -> Array.get idx fields.tiles)
                    |> Maybe.withDefault Wall
        in
        case tile of
            Wall ->
                Wall
                    :: result
                    |> List.reverse

            Empty ->
                let
                    updatedResult =
                        tileAtPosition wantedCoordinate fields
                            :: result
                in
                lookHelp dir wantedCoordinate map updatedResult

            _ ->
                lookHelp
                    dir
                    wantedCoordinate
                    map
                    (tile :: result)


tileAtPosition : Coordinate -> Internals -> Tile
tileAtPosition cord fields =
    let
        playerCoordinates =
            fields.npcs
                |> List.map Tuple.first
                |> List.filter Player.alive
                |> List.map Player.position

        possibleItem =
            fields.items
                |> List.filter (\( itemCord, _ ) -> itemCord == cord)
                |> List.head
                |> Maybe.map Tuple.second
    in
    if List.member cord playerCoordinates then
        Player

    else
        case possibleItem of
            Just item ->
                Item item

            _ ->
                Empty


{-| Ignore this function.
-}
coordinateFrom : Direction -> Coordinate -> Coordinate
coordinateFrom dir start =
    case dir of
        Direction.Left ->
            { start | x = start.x - 1 }

        Direction.Right ->
            { start | x = start.x + 1 }

        Direction.Up ->
            { start | y = start.y - 1 }

        Direction.Down ->
            { start | y = start.y + 1 }


{-| Checks if a Move action can be performed.
-}
canMoveOnto : Coordinate -> Map -> Bool
canMoveOnto cord ((Map fields) as map) =
    if not (coordinatesInBound cord map) then
        False

    else
        let
            tile =
                translateCoordinates cord map
                    |> (\idx -> Array.get idx fields.tiles)
                    |> Maybe.withDefault Wall
        in
        case tile of
            Wall ->
                False

            _ ->
                let
                    playerCoordinates =
                        fields.npcs
                            |> List.map Tuple.first
                            |> List.filter Player.alive
                            |> List.map Player.position
                in
                not <| List.member cord playerCoordinates


{-| Checks if a Move action can be formed onto the given tile.
-}
canMoveOntoTile : Tile -> Bool
canMoveOntoTile tile =
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
