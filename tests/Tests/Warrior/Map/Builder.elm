module Tests.Warrior.Map.Builder exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Internal.Map as Map exposing (Map(..))
import Warrior.Internal.Warrior as Warrior
import Warrior.Item as Item
import Warrior.Map.Builder as Builder
import Warrior.Map.Test
import Warrior.Map.Tile as Tile
import Warrior.Npc.Dummy as Dummy


all : Test
all =
    describe "Warrior.Internal.Builder"
        [ describe "init" initTests
        , describe "armLastNpc" armLastNpcTests
        , describe "npcs" npcsTests
        , describe "spawnPoints" spawnPointsTests
        , describe "withDescription" withDescriptionTests
        , describe "withExitPoint" withExitPointTests
        , describe "withItem" withItemTests
        , describe "withNpc" withNpcTests
        , describe "withSpawnPoint" withSpawnPointTests
        , describe "withWalledArea" withWalledAreaTests
        ]


initTests : List Test
initTests =
    [ test "creates an empty map of the given size" <|
        \() ->
            Builder.init { rows = 4, columns = 3 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    (List.repeat 4 (List.repeat 3 Tile.Empty))
    ]


armLastNpcTests : List Test
armLastNpcTests =
    [ test "gives an item to the most recent NPC" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.withNpc "Robin" { x = 1, y = 1 } Dummy.takeTurn
                |> Builder.armLastNpc Item.Potion
                |> Builder.withNpc "Phill" { x = 1, y = 3 } Dummy.takeTurn
                |> Builder.armLastNpc Item.Sword
                |> Builder.npcs
                |> List.map Tuple.first
                |> Expect.equalLists
                    [ Warrior.spawnVillain "Phill" { x = 1, y = 3 }
                        |> Warrior.addItem Item.Sword
                    , Warrior.spawnVillain "Robin" { x = 1, y = 1 }
                        |> Warrior.addItem Item.Potion
                    ]
    , test "with no NPCs it has no effect" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.armLastNpc Item.Sword
                |> Builder.npcs
                |> Expect.equalLists []
    ]


npcsTests : List Test
npcsTests =
    [ test "returns a list of all NPCs" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.withNpc "Robin" { x = 1, y = 1 } Dummy.takeTurn
                |> Builder.withNpc "Phill" { x = 4, y = 4 } Dummy.takeTurn
                |> Builder.npcs
                |> Expect.equalLists
                    [ ( Warrior.spawnVillain "Phill" { x = 4, y = 4 }, Dummy.takeTurn )
                    , ( Warrior.spawnVillain "Robin" { x = 1, y = 1 }, Dummy.takeTurn )
                    ]
    , test "can include duplicate names" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.withNpc "Dummy" { x = 1, y = 1 } Dummy.takeTurn
                |> Builder.withNpc "Dummy" { x = 3, y = 1 } Dummy.takeTurn
                |> Builder.npcs
                |> Expect.equalLists
                    [ ( Warrior.spawnVillain "Dummy" { x = 3, y = 1 }, Dummy.takeTurn )
                    , ( Warrior.spawnVillain "Dummy" { x = 1, y = 1 }, Dummy.takeTurn )
                    ]
    ]


spawnPointsTests : List Test
spawnPointsTests =
    [ test "returns a list of all Spawn Point's Coordinates" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.withSpawnPoint { x = 1, y = 1 }
                |> Builder.withSpawnPoint { x = 4, y = 4 }
                |> Builder.spawnPoints
                |> Expect.equalLists
                    [ { x = 1, y = 1 }
                    , { x = 4, y = 4 }
                    ]
    , test "ignores any duplicate Spawn Points" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.withSpawnPoint { x = 2, y = 3 }
                |> Builder.withSpawnPoint { x = 2, y = 3 }
                |> Builder.spawnPoints
                |> Expect.equalLists [ { x = 2, y = 3 } ]
    ]


withDescriptionTests : List Test
withDescriptionTests =
    [ test "adds description to the map" <|
        \() ->
            Builder.init { rows = 1, columns = 1 }
                |> Builder.withDescription "A test map."
                |> Builder.build
                |> mapDescription
                |> Expect.equal "A test map."
    ]


withExitPointTests : List Test
withExitPointTests =
    [ test "adds an Exit to the map" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withExitPoint { x = 1, y = 0 }
                |> Builder.build
                |> Map.tileAtPosition { x = 1, y = 0 }
                |> Expect.equal Tile.Exit
    , test "does not add an Exit out of bounds" <|
        \() ->
            Builder.init { rows = 2, columns = 2 }
                |> Builder.withExitPoint { x = 0, y = 3 }
                |> Builder.withExitPoint { x = 3, y = 0 }
                |> Builder.withExitPoint { x = 3, y = 3 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty ]
                    , [ Tile.Empty, Tile.Empty ]
                    ]
    ]


withItemTests : List Test
withItemTests =
    [ test "adds an Item to the map" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withItem { x = 0, y = 0 } Item.Potion
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Item Item.Potion, Tile.Empty, Tile.Empty, Tile.Empty, Tile.Empty ]
                    ]
    , test "replaces existing item at location" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withItem { x = 2, y = 0 } Item.Potion
                |> Builder.withItem { x = 2, y = 0 } Item.Sword
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Item Item.Sword, Tile.Empty, Tile.Empty ]
                    ]
    , test "does not add an Item out of bounds" <|
        \() ->
            Builder.init { rows = 2, columns = 2 }
                |> Builder.withItem { x = 3, y = 0 } Item.Potion
                |> Builder.withItem { x = 0, y = 3 } Item.Sword
                |> Builder.withItem { x = 3, y = 3 } Item.Potion
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty ]
                    , [ Tile.Empty, Tile.Empty ]
                    ]
    ]


withNpcTests : List Test
withNpcTests =
    [ test "places a NPC on the map" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withNpc "Robin" { x = 2, y = 0 } Dummy.takeTurn
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Warrior "Robin", Tile.Empty, Tile.Empty ]
                    ]
    , test "replaces existing NPC at location" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withNpc "Robin" { x = 2, y = 0 } Dummy.takeTurn
                |> Builder.withNpc "Phill" { x = 2, y = 0 } Dummy.takeTurn
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Warrior "Phill", Tile.Empty, Tile.Empty ]
                    ]
    , test "a NPC placed at the same location as another NPC does not appear in the list of NPCs" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withNpc "Robin" { x = 2, y = 0 } Dummy.takeTurn
                |> Builder.withNpc "Phill" { x = 2, y = 0 } Dummy.takeTurn
                |> Builder.npcs
                |> Expect.equalLists
                    [ ( Warrior.spawnVillain "Phill" { x = 2, y = 0 }, Dummy.takeTurn )
                    ]
    , test "a NPC placed out of bounds does not appear on the map" <|
        \() ->
            Builder.init { rows = 2, columns = 2 }
                |> Builder.withNpc "Phill" { x = 3, y = 0 } Dummy.takeTurn
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty ]
                    , [ Tile.Empty, Tile.Empty ]
                    ]
    , test "a NPC placed out of bounds does not appear in list of NPCs" <|
        \() ->
            Builder.init { rows = 2, columns = 2 }
                |> Builder.withNpc "Phill" { x = 3, y = 0 } Dummy.takeTurn
                |> Builder.npcs
                |> Expect.equal []
    ]


withSpawnPointTests : List Test
withSpawnPointTests =
    [ test "adds a SpawnPoint to the map" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withSpawnPoint { x = 3, y = 0 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.SpawnPoint, Tile.Empty ]
                    ]
    , test "does not add a SpawnPoint out of bounds" <|
        \() ->
            Builder.init { rows = 2, columns = 2 }
                |> Builder.withSpawnPoint { x = 3, y = 0 }
                |> Builder.withSpawnPoint { x = 0, y = 3 }
                |> Builder.withSpawnPoint { x = 3, y = 3 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty ]
                    , [ Tile.Empty, Tile.Empty ]
                    ]
    ]


withWalledAreaTests : List Test
withWalledAreaTests =
    [ test "adds a block of Walls to the map" <|
        \() ->
            Builder.init { rows = 4, columns = 4 }
                |> Builder.withWalledArea { x = 1, y = 1 } { x = 2, y = 2 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.Empty ]
                    , [ Tile.Empty, Tile.Wall, Tile.Wall, Tile.Empty ]
                    , [ Tile.Empty, Tile.Wall, Tile.Wall, Tile.Empty ]
                    , [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.Empty ]
                    ]
    , test "does not add Walls outside of map bounds" <|
        \() ->
            Builder.init { rows = 2, columns = 4 }
                |> Builder.withWalledArea { x = 3, y = 0 } { x = 5, y = 3 }
                |> Builder.build
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.Wall ]
                    , [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.Wall ]
                    ]
    ]



--- HELPERS


mapDescription : Map -> String
mapDescription (Map map) =
    map.description
