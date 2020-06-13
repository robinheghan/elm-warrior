module Warrior.Npc.Dummy exposing (takeTurn)

{-| A dummy. A villain which just stands still no matter what happens.

@docs takeTurn

-}

import Warrior exposing (Action(..), Warrior)
import Warrior.History exposing (History)
import Warrior.Map exposing (Map)


{-| Use this function with the `withNPC` function of the `Map` module to add dangerous opponents to a map.
-}
takeTurn : Warrior -> Map -> History -> Action
takeTurn _ _ _ =
    Wait
