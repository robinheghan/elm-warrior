module Warrior.Npc.Dummy exposing (takeTurn)

{-| A dummy. A villainous entity which can be counted on for exactly one thing: standing still.

@docs takeTurn

-}

import Warrior exposing (Action(..), Warrior)
import Warrior.History exposing (History)
import Warrior.Map exposing (Map)


{-| Use this function with the `withNpc` function of the `Warrior.Map.Builder` module to add opponents to a map.
-}
takeTurn : Warrior -> Map -> History -> Action
takeTurn _ _ _ =
    Wait
