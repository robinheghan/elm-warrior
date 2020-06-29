module Tests.Warrior.Npc.Dummy exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior exposing (Warrior)
import Warrior.Internal.History as History exposing (History)
import Warrior.Internal.Warrior as Player
import Warrior.Map exposing (Map)
import Warrior.Map.Builder as Builder
import Warrior.Npc.Dummy as Dummy


all : Test
all =
    describe "Warrior.Npc.Dummy"
        [ describe "takeTurn" takeTurnTests
        ]


takeTurnTests : List Test
takeTurnTests =
    [ test "it waits" <|
        \() ->
            let
                npc : Warrior
                npc =
                    Player.spawnVillain "Dummy" { x = 1, y = 1 }

                map : Map
                map =
                    Builder.init { rows = 3, columns = 3 }
                        |> Builder.withNpc "Dummy" { x = 1, y = 1 } Dummy.takeTurn
                        |> Builder.build
            in
            Dummy.takeTurn npc map emptyHistory
                |> Expect.equal Warrior.Wait
    ]



-- HELPERS


emptyHistory : History
emptyHistory =
    History.init
