module Tests.Warrior.Map.Test exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Internal.Map exposing (Map)
import Warrior.Map.Builder as Builder
import Warrior.Map.Test
import Warrior.Map.Tile as Tile


all : Test
all =
    describe "Warrior.Map.Test"
        [ describe "expectEqualTiles" expectEqualTilesTests
        ]


expectEqualTilesTests : List Test
expectEqualTilesTests =
    [ test "pass" <|
        \() ->
            let
                map : Map
                map =
                    Builder.init { rows = 1, columns = 5 }
                        |> Builder.withSpawnPoint { x = 0, y = 0 }
                        |> Builder.build
            in
            map
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.SpawnPoint, Tile.Empty, Tile.Empty, Tile.Empty, Tile.Empty ]
                    ]
                |> Expect.equal Expect.pass
    , test "fail if any Tile does not match" <|
        \() ->
            let
                map : Map
                map =
                    Builder.init { rows = 1, columns = 5 }
                        |> Builder.withSpawnPoint { x = 0, y = 0 }
                        |> Builder.build
            in
            map
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty, Tile.Empty, Tile.Empty, Tile.SpawnPoint ]
                    ]
                |> Expect.equal (Expect.fail "Expected to find Empty at ( 0, 0 ) but found SpawnPoint instead.")
    , test "expects Wall in missing columns" <|
        \() ->
            let
                map : Map
                map =
                    Builder.init { rows = 2, columns = 2 }
                        |> Builder.build
            in
            map
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty ] ]
                |> Expect.equal (Expect.fail "Expected to find Wall at ( 1, 0 ) but found Empty instead.")
    , test "expects Walls in missing rows" <|
        \() ->
            let
                map : Map
                map =
                    Builder.init { rows = 2, columns = 2 }
                        |> Builder.build
            in
            map
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Empty ]
                    ]
                |> Expect.equal (Expect.fail "Expected to find Wall at ( 0, 1 ) but found Empty instead.")
    , test "finds Walls when there are too many rows or columns expected" <|
        \() ->
            let
                map : Map
                map =
                    Builder.init { rows = 1, columns = 1 }
                        |> Builder.build
            in
            map
                |> Warrior.Map.Test.expectEqualTiles
                    [ [ Tile.Empty, Tile.Wall, Tile.Wall ]
                    , [ Tile.Wall, Tile.Wall, Tile.Wall ]
                    ]
    ]
