module Tests.Warrior.Internal.Map exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Direction as Direction
import Warrior.Internal.Map as Map exposing (Map)
import Warrior.Internal.Warrior as Player
import Warrior.Item as Item exposing (Item)
import Warrior.Map.Builder as Builder
import Warrior.Map.Test
import Warrior.Map.Tile as Tile
import Warrior.Npc.Dummy as Dummy


all : Test
all =
    describe "Warrior.Internal.Map"
        [ describe "coordinateFrom" coordinateFromTests
        , describe "look" lookTests
        , describe "lookDown" lookDownTests
        , describe "removeItem" removeItemTests
        , describe "setNpcs" setNpcsTests
        , describe "spawnPoints" spawnPointTests
        , describe "tileAtPosition" tileAtPositionTests
        ]


coordinateFromTests : List Test
coordinateFromTests =
    [ test "gets coordinate to the Left" <|
        \() ->
            { x = 2, y = 2 }
                |> Map.coordinateFrom Direction.Left
                |> Expect.equal { x = 1, y = 2 }
    , test "gets coordinate to the Right" <|
        \() ->
            { x = 2, y = 2 }
                |> Map.coordinateFrom Direction.Right
                |> Expect.equal { x = 3, y = 2 }
    , test "gets coordinate above (Up)" <|
        \() ->
            { x = 2, y = 2 }
                |> Map.coordinateFrom Direction.Up
                |> Expect.equal { x = 2, y = 1 }
    , test "gets coordinate below (Down)" <|
        \() ->
            { x = 2, y = 2 }
                |> Map.coordinateFrom Direction.Down
                |> Expect.equal { x = 2, y = 3 }
    ]


lookTests : List Test
lookTests =
    [ describe "Up"
        [ test "is a list of tiles from the warrior's coordinate to the Boundary" <|
            \() ->
                Builder.init { rows = 5, columns = 5 }
                    |> Builder.withSpawnPoint { x = 1, y = 0 }
                    |> Builder.withItem { x = 1, y = 2 } Item.Sword
                    |> Builder.withNpc "Dummy" { x = 1, y = 3 } Dummy.takeTurn
                    |> Builder.withItem { x = 3, y = 3 } Item.Potion
                    |> Builder.build
                    |> Map.look Direction.Up (Player.spawnHero "Player" { x = 1, y = 4 })
                    |> Expect.equalLists
                        [ ( { x = 1, y = 3 }, Tile.Warrior "Dummy" )
                        , ( { x = 1, y = 2 }, Tile.Item Item.Sword )
                        , ( { x = 1, y = 1 }, Tile.Empty )
                        , ( { x = 1, y = 0 }, Tile.SpawnPoint )
                        , ( { x = 1, y = -1 }, Tile.Wall )
                        ]
        , test "it looks as far as the nearest Wall" <|
            \() ->
                Builder.init { rows = 5, columns = 5 }
                    |> Builder.withSpawnPoint { x = 1, y = 0 }
                    |> Builder.withWalledArea { x = 0, y = 1 } { x = 4, y = 0 }
                    |> Builder.withItem { x = 1, y = 2 } Item.Sword
                    |> Builder.withNpc "Dummy" { x = 1, y = 3 } Dummy.takeTurn
                    |> Builder.build
                    |> Map.look Direction.Up (Player.spawnHero "Player" { x = 1, y = 4 })
                    |> Expect.equalLists
                        [ ( { x = 1, y = 3 }, Tile.Warrior "Dummy" )
                        , ( { x = 1, y = 2 }, Tile.Item Item.Sword )
                        , ( { x = 1, y = 1 }, Tile.Wall )
                        ]
        ]
    , describe "Right" <|
        [ test "is a list of tiles from the warrior's coordinate to the Boundary" <|
            \() ->
                Builder.init { rows = 5, columns = 5 }
                    |> Builder.withExitPoint { x = 4, y = 1 }
                    |> Builder.withNpc "Dummy" { x = 3, y = 1 } Dummy.takeTurn
                    |> Builder.withItem { x = 1, y = 1 } Item.Potion
                    |> Builder.withItem { x = 3, y = 3 } Item.Sword
                    |> Builder.build
                    |> Map.look Direction.Right (Player.spawnHero "Player" { x = 0, y = 1 })
                    |> Expect.equalLists
                        [ ( { x = 1, y = 1 }, Tile.Item Item.Potion )
                        , ( { x = 2, y = 1 }, Tile.Empty )
                        , ( { x = 3, y = 1 }, Tile.Warrior "Dummy" )
                        , ( { x = 4, y = 1 }, Tile.Exit )
                        , ( { x = 5, y = 1 }, Tile.Wall )
                        ]
        , test "it looks as far as the nearest Wall" <|
            \() ->
                Builder.init { rows = 5, columns = 5 }
                    |> Builder.withExitPoint { x = 4, y = 1 }
                    |> Builder.withNpc "Dummy" { x = 3, y = 1 } Dummy.takeTurn
                    |> Builder.withWalledArea { x = 2, y = 0 } { x = 2, y = 4 }
                    |> Builder.withItem { x = 1, y = 1 } Item.Potion
                    |> Builder.withItem { x = 3, y = 3 } Item.Sword
                    |> Builder.build
                    |> Map.look Direction.Right (Player.spawnHero "Player" { x = 0, y = 1 })
                    |> Expect.equalLists
                        [ ( { x = 1, y = 1 }, Tile.Item Item.Potion )
                        , ( { x = 2, y = 1 }, Tile.Wall )
                        ]
        ]
    , describe "Down" <|
        [ test "is a list of tiles from the warrior's coordinate to the Boundary" <|
            \() ->
                Builder.init { rows = 5, columns = 1 }
                    |> Builder.withItem { x = 0, y = 1 } Item.Potion
                    |> Builder.withItem { x = 0, y = 2 } Item.Sword
                    |> Builder.withExitPoint { x = 0, y = 4 }
                    |> Builder.build
                    |> Map.look Direction.Down (Player.spawnHero "Player" { x = 0, y = 0 })
                    |> Expect.equalLists
                        [ ( { x = 0, y = 1 }, Tile.Item Item.Potion )
                        , ( { x = 0, y = 2 }, Tile.Item Item.Sword )
                        , ( { x = 0, y = 3 }, Tile.Empty )
                        , ( { x = 0, y = 4 }, Tile.Exit )
                        , ( { x = 0, y = 5 }, Tile.Wall )
                        ]
        , test "it looks as far as the nearest Wall" <|
            \() ->
                Builder.init { rows = 5, columns = 1 }
                    |> Builder.withItem { x = 0, y = 1 } Item.Potion
                    |> Builder.withItem { x = 0, y = 2 } Item.Sword
                    |> Builder.withWalledArea { x = 0, y = 3 } { x = 0, y = 3 }
                    |> Builder.withExitPoint { x = 0, y = 4 }
                    |> Builder.build
                    |> Map.look Direction.Down (Player.spawnHero "Player" { x = 0, y = 0 })
                    |> Expect.equalLists
                        [ ( { x = 0, y = 1 }, Tile.Item Item.Potion )
                        , ( { x = 0, y = 2 }, Tile.Item Item.Sword )
                        , ( { x = 0, y = 3 }, Tile.Wall )
                        ]
        ]
    , describe "Left" <|
        [ test "is a list of tiles from the warrior's coordinate to the Boundary" <|
            \() ->
                Builder.init { rows = 1, columns = 7 }
                    |> Builder.withSpawnPoint { x = 6, y = 0 }
                    |> Builder.withItem { x = 3, y = 0 } Item.Sword
                    |> Builder.withNpc "Dummy" { x = 1, y = 0 } Dummy.takeTurn
                    |> Builder.withExitPoint { x = 0, y = 0 }
                    |> Builder.build
                    |> Map.look Direction.Left (Player.spawnHero "Player" { x = 6, y = 0 })
                    |> Expect.equalLists
                        [ ( { x = 5, y = 0 }, Tile.Empty )
                        , ( { x = 4, y = 0 }, Tile.Empty )
                        , ( { x = 3, y = 0 }, Tile.Item Item.Sword )
                        , ( { x = 2, y = 0 }, Tile.Empty )
                        , ( { x = 1, y = 0 }, Tile.Warrior "Dummy" )
                        , ( { x = 0, y = 0 }, Tile.Exit )
                        , ( { x = -1, y = 0 }, Tile.Wall )
                        ]
        , test "it looks as far as the nearest Wall" <|
            \() ->
                Builder.init { rows = 1, columns = 7 }
                    |> Builder.withSpawnPoint { x = 6, y = 0 }
                    |> Builder.withItem { x = 3, y = 0 } Item.Sword
                    |> Builder.withWalledArea { x = 2, y = 0 } { x = 2, y = 0 }
                    |> Builder.withNpc "Dummy" { x = 1, y = 0 } Dummy.takeTurn
                    |> Builder.withExitPoint { x = 0, y = 0 }
                    |> Builder.build
                    |> Map.look Direction.Left (Player.spawnHero "Player" { x = 6, y = 0 })
                    |> Expect.equalLists
                        [ ( { x = 5, y = 0 }, Tile.Empty )
                        , ( { x = 4, y = 0 }, Tile.Empty )
                        , ( { x = 3, y = 0 }, Tile.Item Item.Sword )
                        , ( { x = 2, y = 0 }, Tile.Wall )
                        ]
        ]
    , test "it looks from the position of the NPC known by the map" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.withItem { x = 1, y = 0 } Item.Potion
                |> Builder.withExitPoint { x = 1, y = 1 }
                |> Builder.withNpc "Dummy" { x = 1, y = 3 } Dummy.takeTurn
                |> Builder.build
                |> Map.look Direction.Up (Player.spawnVillain "Dummy" { x = 0, y = 0 })
                |> Expect.equalLists
                    [ ( { x = 1, y = 2 }, Tile.Empty )
                    , ( { x = 1, y = 1 }, Tile.Exit )
                    , ( { x = 1, y = 0 }, Tile.Item Item.Potion )
                    , ( { x = 1, y = -1 }, Tile.Wall )
                    ]
    , test "if a Warrior is standing on an Item, it sees the Warrior" <|
        \() ->
            Builder.init { rows = 2, columns = 1 }
                |> Builder.withItem { x = 0, y = 1 } Item.Potion
                |> Builder.withNpc "Dummy" { x = 0, y = 1 } Dummy.takeTurn
                |> Builder.build
                |> Map.look Direction.Down (Player.spawnHero "Player" { x = 0, y = 0 })
                |> Expect.equalLists
                    [ ( { x = 0, y = 1 }, Tile.Warrior "Dummy" )
                    , ( { x = 0, y = 2 }, Tile.Wall )
                    ]
    ]


lookDownTests : List Test
lookDownTests =
    [ describe "SpawnPoint"
        [ test "when the player is on a Spawn Point" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 0, y = 0 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withSpawnPoint { x = 0, y = 0 }
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal Tile.SpawnPoint
        ]
    , describe "Exit"
        [ test "when the player is on an Exit" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 4, y = 0 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withExitPoint { x = 4, y = 0 }
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal Tile.Exit
        ]
    , describe "Empty"
        [ test "when player is on an Empty tile with no items" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 2, y = 0 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal Tile.Empty
        ]
    , describe "Item"
        [ test "when player is on an Empty tile with an Item" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 1, y = 0 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withItem { x = 1, y = 0 } Item.Potion
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal (Tile.Item Item.Potion)
        ]
    , describe "Wall"
        [ test "when player is not on the map" <|
            \() ->
                let
                    player : Player.Warrior
                    player =
                        Player.spawnHero "Tester" { x = 1, y = 9 }
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.build
                    |> Map.lookDown player
                    |> Expect.equal Tile.Wall
        ]
    ]


removeItemTests : List Test
removeItemTests =
    [ test "with coordinates of item, it is the item and map without item" <|
        \() ->
            let
                result : Maybe ( Item, Map )
                result =
                    Builder.init { rows = 1, columns = 5 }
                        |> Builder.withItem { x = 3, y = 0 } Item.Potion
                        |> Builder.build
                        |> Map.removeItem { x = 3, y = 0 }
            in
            case result of
                Just ( item, map ) ->
                    ( item, Map.tileAtPosition { x = 3, y = 0 } map )
                        |> Expect.equal ( Item.Potion, Tile.Empty )

                Nothing ->
                    Expect.fail "Expected Just but got Nothing"
    , test "with coordinates of no item, it is Nothing" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.build
                |> Map.removeItem { x = 3, y = 0 }
                |> Expect.equal Nothing
    ]


setNpcsTests : List Test
setNpcsTests =
    [ test "given no NPCs removes all NPCs on map" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withNpc "Dummy 1" { x = 4, y = 0 } Dummy.takeTurn
                |> Builder.withNpc "Dummy 2" { x = 1, y = 0 } Dummy.takeTurn
                |> Builder.build
                |> Map.setNpcs []
                |> Warrior.Map.Test.expectEqualTiles
                    [ List.repeat 5 Tile.Empty ]
    , test "replaces NPCs on the map" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.withNpc "Dummy" { x = 4, y = 0 } Dummy.takeTurn
                |> Builder.build
                |> Map.setNpcs
                    [ Player.spawnVillain "Player" { x = 1, y = 0 }
                    ]
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Warrior "Player", Tile.Empty, Tile.Empty, Tile.Empty ]
                    ]
    , test "moves NPCs to new coordinates" <|
        \() ->
            Builder.init { rows = 5, columns = 1 }
                |> Builder.withNpc "Dummy" { x = 0, y = 4 } Dummy.takeTurn
                |> Builder.build
                |> Map.setNpcs
                    [ Player.spawnVillain "Dummy" { x = 0, y = 3 }
                    ]
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty ]
                    , [ Tile.Empty ]
                    , [ Tile.Empty ]
                    , [ Tile.Warrior "Dummy" ]
                    , [ Tile.Empty ]
                    ]
    ]


spawnPointTests : List Test
spawnPointTests =
    [ test "is a list of all spawn points' coordinates" <|
        \() ->
            Builder.init { rows = 5, columns = 5 }
                |> Builder.withSpawnPoint { x = 1, y = 3 }
                |> Builder.withSpawnPoint { x = 2, y = 4 }
                |> Builder.withSpawnPoint { x = 3, y = 1 }
                |> Builder.build
                |> Map.spawnPoints
                |> expectListContainsAll
                    [ { x = 1, y = 3 }
                    , { x = 2, y = 4 }
                    , { x = 3, y = 1 }
                    ]
    , test "with no spawn points is empty" <|
        \() ->
            Builder.init { rows = 1, columns = 5 }
                |> Builder.build
                |> Map.spawnPoints
                |> Expect.equal []
    ]


tileAtPositionTests : List Test
tileAtPositionTests =
    [ describe "SpawnPoint"
        [ test "when the coordinate is a Spawn Point" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withSpawnPoint { x = 0, y = 0 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 0, y = 0 }
                    |> Expect.equal Tile.SpawnPoint
        ]
    , describe "Exit"
        [ test "when the coordinate is an Exit" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withExitPoint { x = 4, y = 0 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 4, y = 0 }
                    |> Expect.equal Tile.Exit
        ]
    , describe "Empty"
        [ test "when the coordinate is an Empty tile with no item or NPC" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 2, y = 0 }
                    |> Expect.equal Tile.Empty
        , test "when the coordinate is an Empty tile with a dead NPC" <|
            \() ->
                let
                    dummy : Player.Warrior
                    dummy =
                        Player.spawnVillain "Dummy" { x = 4, y = 0 }
                            |> injurePlayerBy 10
                in
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withNpc "Dummy" { x = 4, y = 0 } Dummy.takeTurn
                    |> Builder.build
                    |> Map.setNpcs [ dummy ]
                    |> Map.tileAtPosition { x = 4, y = 0 }
                    |> Expect.equal Tile.Empty
        ]
    , describe "Item"
        [ test "when the coordinate is an Empty tile with an Item and no NPC" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withItem { x = 1, y = 0 } Item.Potion
                    |> Builder.build
                    |> Map.tileAtPosition { x = 1, y = 0 }
                    |> Expect.equal (Tile.Item Item.Potion)
        ]
    , describe "Wall"
        [ test "when the coordinate is a Wall" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withWalledArea { x = 1, y = 0 } { x = 3, y = 0 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 2, y = 0 }
                    |> Expect.equal Tile.Wall
        , test "when the coordinate is not on the map" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.build
                    |> Map.tileAtPosition { x = 1, y = 9 }
                    |> Expect.equal Tile.Wall
        ]
    , describe "Warrior"
        [ test "when the coordinate is an Empty tile with a Warrior" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withNpc "Dummy" { x = 3, y = 0 } Dummy.takeTurn
                    |> Builder.build
                    |> Map.tileAtPosition { x = 3, y = 0 }
                    |> Expect.equal (Tile.Warrior "Dummy")
        , test "when the coordinate is an Empty tile with an Item and a Warrior" <|
            \() ->
                Builder.init { rows = 1, columns = 5 }
                    |> Builder.withItem { x = 4, y = 0 } Item.Sword
                    |> Builder.withNpc "Dummy" { x = 4, y = 0 } Dummy.takeTurn
                    |> Builder.build
                    |> Map.tileAtPosition { x = 4, y = 0 }
                    |> Expect.equal (Tile.Warrior "Dummy")
        ]
    ]



--- HELPERS


injurePlayerBy : Int -> Player.Warrior -> Player.Warrior
injurePlayerBy damage player =
    let
        attacker : Player.Warrior
        attacker =
            Player.spawnVillain "Attacker" { x = 0, y = 0 }
    in
    List.repeat damage ()
        |> List.foldl (\() -> Player.attack attacker) player


{-| Passes if the lists contain all of the same items in any order.
-}
expectListContainsAll : List a -> List a -> Expect.Expectation
expectListContainsAll expected actual =
    Expect.all
        ((List.length >> Expect.equal (List.length expected))
            :: List.map expectListContains expected
        )
        actual


{-| Passes if the list contains the given item.
-}
expectListContains : a -> List a -> Expect.Expectation
expectListContains value list =
    List.member value list
        |> Expect.true ("Expected list to contain " ++ Debug.toString value ++ ".")
