module Warrior.Internal.Map exposing
    ( Map(..)
    , canMoveOnto, canMoveOntoTile, isExitPoint, look, lookDown
    , coordinateFrom, setNpcs, removeItem, spawnPoints, view
    )

{-| The functions in this module allows you to create your own maps, or simply ask questions about the map currently being played.

@docs Map


# Player API

@docs canMoveOnto, canMoveOntoTile, isExitPoint, look, lookDown


# Internals

@docs coordinateFrom, setNpcs, removeItem, spawnPoints, view

-}

import Array exposing (Array)
import Color exposing (Color)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import List.Extra as List
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction as Direction exposing (Direction)
import Warrior.Item exposing (Item)
import Warrior.Player as Player exposing (Player)
import Warrior.Tile exposing (Tile(..))


{-| A map, or level.
-}
type Map
    = Map Internals


type alias Internals =
    { tilesPerRow : Int
    , tiles : Array Tile
    , items : List ( Coordinate, Item )
    , npcs : List Player
    , description : String
    }


{-| Replace all villains with the provided list. Might be less code than using multiple `withNPC` calls.
-}
setNpcs : List Player -> Map -> Map
setNpcs newNpcs (Map fields) =
    Map { fields | npcs = newNpcs }


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


{-| Checks to see if a specific coordinate on the map is an exit point.
-}
isExitPoint : Map -> Coordinate -> Bool
isExitPoint ((Map fields) as map) cord =
    Array.get (translateCoordinates cord map) fields.tiles
        |> Maybe.map ((==) Exit)
        |> Maybe.withDefault False


{-| Framework function
-}
view : List ( Coordinate, Color ) -> Map -> Element a
view playerPositions ((Map fields) as map) =
    let
        playerColors =
            List.map (\( cord, clr ) -> ( translateCoordinates cord map, clr )) playerPositions
                |> Dict.fromList

        firstAndLast =
            Array.initialize (fields.tilesPerRow + 2) (always Wall)
                |> Array.map (viewTile Dict.empty 0)
                |> Array.toList
                |> Element.row []

        withDesc mapElement =
            Element.column
                []
                [ Element.paragraph
                    [ Element.paddingEach
                        { top = 0
                        , left = 0
                        , right = 0
                        , bottom = 50
                        }
                    , Element.width <| Element.maximum 500 Element.fill
                    , Element.centerX
                    ]
                    [ Element.text fields.description ]
                , mapElement
                ]
    in
    fields.tiles
        |> Array.indexedMap (mapTile fields)
        |> Array.indexedMap (viewTile playerColors)
        |> asRows fields.tilesPerRow []
        |> surroundElements firstAndLast
        |> Element.column [ Element.centerX ]
        |> withDesc


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


viewTile : Dict Int Color -> Int -> Tile -> Element a
viewTile playerColors index tile =
    Element.el
        [ Element.width <| Element.px 50
        , Element.height <| Element.px 50
        , Background.color <| tileColor tile
        , Border.width 1
        ]
        (case Dict.get index playerColors of
            Just playerColor ->
                let
                    ( r, g, b ) =
                        Color.toRGB playerColor
                in
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    , Font.color <| Element.rgb255 (floor r) (floor g) (floor b)
                    ]
                    (Element.text "@")

            Nothing ->
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
                    |> surroundElements (viewTile Dict.empty 0 Wall)
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

        Player _ ->
            Element.rgb255 255 255 255

        Item _ ->
            Element.rgba255 255 255 0 0.4

        SpawnPoint ->
            Element.rgba255 0 0 255 0.4

        Exit ->
            Element.rgba255 0 255 0 0.4



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


{-| Describe what is at the players feet. Useful for deciding if there's an item worth picking up.
-}
lookDown : Player -> Map -> Tile
lookDown player ((Map fields) as map) =
    let
        playerPosition =
            Player.position player

        tile =
            playerPosition
                |> (\c -> translateCoordinates c map)
                |> (\idx -> Array.get idx fields.tiles)
                |> Maybe.withDefault Wall
    in
    case tile of
        Empty ->
            fields.items
                |> List.filter (\( itemCord, _ ) -> itemCord == playerPosition)
                |> List.head
                |> Maybe.map (Item << Tuple.second)
                |> Maybe.withDefault Empty

        _ ->
            tile


tileAtPosition : Coordinate -> Internals -> Tile
tileAtPosition cord fields =
    let
        possiblePlayer =
            fields.npcs
                |> List.filter Player.alive
                |> List.find (\player -> Player.position player == cord)

        possibleItem =
            fields.items
                |> List.find (\( itemCord, _ ) -> itemCord == cord)
                |> Maybe.map Tuple.second
    in
    case ( possiblePlayer, possibleItem ) of
        ( Just player, _ ) ->
            Player (Player.id player)

        ( _, Just item ) ->
            Item item

        ( Nothing, Nothing ) ->
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
