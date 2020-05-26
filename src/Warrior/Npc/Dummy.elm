module Warrior.Npc.Dummy exposing (takeTurn)

import Warrior.Map exposing (Map)
import Warrior.Player exposing (Action(..), Player)


takeTurn : Player -> Map -> Action
takeTurn _ _ =
    Wait
