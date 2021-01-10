module Tests.Warrior.Direction exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Direction as Direction


all : Test
all =
    describe "Warrior.Direction"
        [ describe "all" allTests
        , describe "toString" toStringTests
        ]


allTests : List Test
allTests =
    [ test "contains all directions" <|
        \() ->
            Direction.all
                |> expectListContainsAll
                    [ Direction.Left
                    , Direction.Up
                    , Direction.Right
                    , Direction.Down
                    ]
    ]


toStringTests : List Test
toStringTests =
    [ test "formats Left" <|
        \() ->
            Direction.toString Direction.Left
                |> Expect.equal "Left"
    , test "formats Up" <|
        \() ->
            Direction.toString Direction.Up
                |> Expect.equal "Up"
    , test "formats Right" <|
        \() ->
            Direction.toString Direction.Right
                |> Expect.equal "Right"
    , test "formats Down" <|
        \() ->
            Direction.toString Direction.Down
                |> Expect.equal "Down"
    ]


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
