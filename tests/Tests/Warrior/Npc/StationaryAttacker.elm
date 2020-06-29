module Tests.Warrior.Npc.StationaryAttacker exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior exposing (Warrior)
import Warrior.Direction as Direction
import Warrior.Internal.History as History exposing (History)
import Warrior.Internal.Map as Map exposing (Map)
import Warrior.Internal.Warrior as Player
import Warrior.Map.Builder as Builder
import Warrior.Npc.StationaryAttacker as StationaryAttacker


all : Test
all =
    describe "Warrior.Npc.StationaryAttacker"
        [ describe "takeTurn" takeTurnTests
        ]


takeTurnTests : List Test
takeTurnTests =
    [ test "with no player adjacent it waits" <|
        \() ->
            let
                guard : Warrior
                guard =
                    Player.spawnVillain "Guard" { x = 1, y = 1 }

                player : Warrior
                player =
                    Player.spawnHero "Player" { x = 0, y = 0 }

                map : Map
                map =
                    Builder.init { rows = 3, columns = 3 }
                        |> Builder.withNpc "Guard" { x = 1, y = 1 } StationaryAttacker.takeTurn
                        |> Builder.build
                        |> Map.setNpcs [ guard, player ]
            in
            StationaryAttacker.takeTurn guard map emptyHistory
                |> Expect.equal Warrior.Wait
    , test "with a player above it attacks up" <|
        \() ->
            let
                guard : Warrior
                guard =
                    Player.spawnVillain "Guard" { x = 1, y = 1 }

                player : Warrior
                player =
                    Player.spawnHero "Player" { x = 1, y = 0 }

                map : Map
                map =
                    Builder.init { rows = 3, columns = 3 }
                        |> Builder.withNpc "Guard" { x = 1, y = 1 } StationaryAttacker.takeTurn
                        |> Builder.build
                        |> Map.setNpcs [ guard, player ]
            in
            StationaryAttacker.takeTurn guard map emptyHistory
                |> Expect.equal (Warrior.Attack Direction.Up)
    , test "with a player to the right it attacks right" <|
        \() ->
            let
                guard : Warrior
                guard =
                    Player.spawnVillain "Guard" { x = 1, y = 1 }

                player : Warrior
                player =
                    Player.spawnHero "Player" { x = 2, y = 1 }

                map : Map
                map =
                    Builder.init { rows = 3, columns = 3 }
                        |> Builder.withNpc "Guard" { x = 1, y = 1 } StationaryAttacker.takeTurn
                        |> Builder.build
                        |> Map.setNpcs [ guard, player ]
            in
            StationaryAttacker.takeTurn guard map emptyHistory
                |> Expect.equal (Warrior.Attack Direction.Right)
    , test "with a player below it attacks down" <|
        \() ->
            let
                guard : Warrior
                guard =
                    Player.spawnVillain "Guard" { x = 1, y = 1 }

                player : Warrior
                player =
                    Player.spawnHero "Player" { x = 1, y = 2 }

                map : Map
                map =
                    Builder.init { rows = 3, columns = 3 }
                        |> Builder.withNpc "Guard" { x = 1, y = 1 } StationaryAttacker.takeTurn
                        |> Builder.build
                        |> Map.setNpcs [ guard, player ]
            in
            StationaryAttacker.takeTurn guard map emptyHistory
                |> Expect.equal (Warrior.Attack Direction.Down)
    , test "with a player to the left it attacks left" <|
        \() ->
            let
                guard : Warrior
                guard =
                    Player.spawnVillain "Guard" { x = 1, y = 1 }

                player : Warrior
                player =
                    Player.spawnHero "Player" { x = 0, y = 1 }

                map : Map
                map =
                    Builder.init { rows = 3, columns = 3 }
                        |> Builder.withNpc "Guard" { x = 1, y = 1 } StationaryAttacker.takeTurn
                        |> Builder.build
                        |> Map.setNpcs [ guard, player ]
            in
            StationaryAttacker.takeTurn guard map emptyHistory
                |> Expect.equal (Warrior.Attack Direction.Left)
    ]



-- HELPERS


emptyHistory : History
emptyHistory =
    History.init
