module Warrior.History exposing
    ( History
    , previousActions
    , previousStates
    )

import Warrior.Internal.History as Internal exposing (History)
import Warrior.Map exposing (Map)
import Warrior.Player as Player exposing (Player)


type alias History =
    Internal.History


previousStates : Player -> History -> List ( Player, Map )
previousStates =
    Internal.previousStates


previousActions : Player -> History -> List Player.Action
previousActions =
    Internal.previousActions
