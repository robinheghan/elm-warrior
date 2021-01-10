module Tests.Warrior.Coordinate exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Coordinate as Coordinate


all : Test
all =
    describe "Warrior.Coordinate"
        [ describe "toString" toStringTests
        ]


toStringTests : List Test
toStringTests =
    [ test "formats coordinates" <|
        \() ->
            { x = 1, y = 2 }
                |> Coordinate.toString
                |> Expect.equal "( 1, 2 )"
    ]
