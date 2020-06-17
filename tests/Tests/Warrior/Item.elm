module Tests.Warrior.Item exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Item as Item


all : Test
all =
    describe "Warrior.Item"
        [ describe "toString" toStringTests
        ]


toStringTests : List Test
toStringTests =
    [ test "formats Potion" <|
        \() ->
            Item.toString Item.Potion
                |> Expect.equal "Potion"
    , test "formats Sword" <|
        \() ->
            Item.toString Item.Sword
                |> Expect.equal "Sword"
    ]
