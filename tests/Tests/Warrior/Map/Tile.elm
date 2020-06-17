module Tests.Warrior.Map.Tile exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Item as Item
import Warrior.Map.Tile as Tile


all : Test
all =
    describe "Warrior.Map.Tile"
        [ describe "isWall" isWallTests
        , describe "isEmpty" isEmptyTests
        , describe "isSpawnPoint" isSpawnPointTests
        , describe "isExit" isExitTests
        , describe "isWarrior" isWarriorTests
        , describe "isItem" isItemTests
        , describe "canMoveOnto" canMoveOntoTests
        ]


isWallTests : List Test
isWallTests =
    [ test "with a Wall is True" <|
        \() ->
            Tile.isWall Tile.Wall
                |> Expect.equal True
    , test "with a non-Wall tile is False" <|
        \() ->
            Tile.isWall Tile.Empty
                |> Expect.equal False
    ]


isEmptyTests : List Test
isEmptyTests =
    [ test "with an Empty is True" <|
        \() ->
            Tile.isEmpty Tile.Empty
                |> Expect.equal True
    , test "with a non-Empty tile is False" <|
        \() ->
            Tile.isEmpty Tile.SpawnPoint
                |> Expect.equal False
    ]


isSpawnPointTests : List Test
isSpawnPointTests =
    [ test "with a Spawn Point is True" <|
        \() ->
            Tile.isSpawnPoint Tile.SpawnPoint
                |> Expect.equal True
    , test "with a non-Spawn Point tile is False" <|
        \() ->
            Tile.isSpawnPoint Tile.Exit
                |> Expect.equal False
    ]


isExitTests : List Test
isExitTests =
    [ test "with an Exit is True" <|
        \() ->
            Tile.isExit Tile.Exit
                |> Expect.equal True
    , test "with a non-Exit tile is False" <|
        \() ->
            Tile.isExit (Tile.Warrior "Robin")
                |> Expect.equal False
    ]


isWarriorTests : List Test
isWarriorTests =
    [ test "with a Warrior is True" <|
        \() ->
            Tile.isWarrior (Tile.Warrior "Phill")
                |> Expect.equal True
    , test "with a non-Warrior tile is False" <|
        \() ->
            Tile.isWarrior (Tile.Item Item.Potion)
                |> Expect.equal False
    ]


isItemTests : List Test
isItemTests =
    [ test "with an Item is True" <|
        \() ->
            Tile.isItem (Tile.Item Item.Sword)
                |> Expect.equal True
    , test "with a non-Item tile is False" <|
        \() ->
            Tile.isItem Tile.Wall
                |> Expect.equal False
    ]


canMoveOntoTests : List Test
canMoveOntoTests =
    [ test "with a Wall is False" <|
        \() ->
            Tile.canMoveOnto Tile.Wall
                |> Expect.equal False
    , test "with an Empty is True" <|
        \() ->
            Tile.canMoveOnto Tile.Empty
                |> Expect.equal True
    , test "with a Spawn Point is True" <|
        \() ->
            Tile.canMoveOnto Tile.SpawnPoint
                |> Expect.equal True
    , test "with an Exit is True" <|
        \() ->
            Tile.canMoveOnto Tile.Exit
                |> Expect.equal True
    , test "with a Warrior is False" <|
        \() ->
            Tile.canMoveOnto (Tile.Warrior "Evan")
                |> Expect.equal False
    , test "with an Item is True" <|
        \() ->
            Tile.canMoveOnto (Tile.Item Item.Potion)
                |> Expect.equal True
    ]
