module Tests.Warrior exposing (all)

import Expect
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Warrior"
        -- TODO: Add Warrior tests
        [ test "package compiles" <|
            \() ->
                Expect.pass
        ]
