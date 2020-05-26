module Warrior.Map exposing
    ( Config
    , Map
    , Tile(..)
    , armLastNpc
    , canMoveOnto
    , canMoveOntoTile
    , coordinateFrom
    , init
    , isExitPoint
    , look
    , npcs
    , removeItem
    , setNpcs
    , spawnPoints
    , view
    , withExitPoint
    , withItem
    , withNPC
    , withSpawnPoint
    , withWalledArea
    )

import Array exposing (Array)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.Item exposing (Item)
import Warrior.Player as Player exposing (Player)


type Map
    = Map Internals


type alias Internals =
    { tilesPerRow : Int
    , tiles : Array Tile
    , items : List ( Coordinate, Item )
    , npcs : List ( Player, Player -> Map -> Player.Action )
    }


type Tile
    = Wall
    | Empty
    | SpawnPoint
    | Player
    | Item Item
    | Exit


type alias Config =
    { rows : Int
    , columns : Int
    }


init : Config -> Map
init config =
    Map
        { tilesPerRow = config.columns
        , tiles = Array.initialize (config.columns * config.rows) (always Empty)
        , items = []
        , npcs = []
        }


withSpawnPoint : Coordinate -> Map -> Map
withSpawnPoint cord map =
    if coordinatesInBound cord map then
        updateTiles (Array.set (translateCoordinates cord map) SpawnPoint) map

    else
        map


withExitPoint : Coordinate -> Map -> Map
withExitPoint cord map =
    if coordinatesInBound cord map then
        updateTiles (Array.set (translateCoordinates cord map) Exit) map

    else
        map


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


withNPC : Coordinate -> (Player -> Map -> Player.Action) -> Map -> Map
withNPC cord turnFunc (Map fields) =
    Map { fields | npcs = ( Player.spawnVillain cord, turnFunc ) :: fields.npcs }


setNpcs : List ( Player, Player -> Map -> Player.Action ) -> Map -> Map
setNpcs newNpcs (Map fields) =
    Map { fields | npcs = newNpcs }


armLastNpc : Item -> Map -> Map
armLastNpc item (Map fields) =
    case fields.npcs of
        [] ->
            Map fields

        ( lastNpcState, lastNpcBrain ) :: rest ->
            Map { fields | npcs = ( Player.addItem item lastNpcState, lastNpcBrain ) :: rest }


withItem : Coordinate -> Item -> Map -> Map
withItem coordinate item (Map fields) =
    let
        cleansedItems =
            List.filter (\( itemCord, _ ) -> itemCord /= coordinate) fields.items
    in
    Map { fields | items = ( coordinate, item ) :: cleansedItems }


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


spawnPoints : Map -> List Coordinate
spawnPoints (Map fields) =
    Array.indexedMap Tuple.pair fields.tiles
        |> Array.filter (\( _, tile ) -> tile == SpawnPoint)
        |> Array.map (Tuple.first >> indexToCoordinate fields)
        |> Array.toList


npcs : Map -> List ( Player, Player -> Map -> Player.Action )
npcs (Map fields) =
    fields.npcs


isExitPoint : Map -> Coordinate -> Bool
isExitPoint ((Map fields) as map) cord =
    Array.get (translateCoordinates cord map) fields.tiles
        |> Maybe.map ((==) Exit)
        |> Maybe.withDefault False


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
                |> List.map Player.currentPosition

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
                            |> List.map Player.currentPosition
                in
                not <| List.member cord playerCoordinates


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
