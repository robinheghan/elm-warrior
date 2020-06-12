module Warrior.Player exposing
    ( Player, Action(..)
    , id, position, health, attackDamage, healingPotential, inventory, maxHealth
    )

{-| The player record contains all known state for playable, as well as non-playable characters in the game. You'll probably need the functions in this module to make decisions about your next turn.

@docs Player, Action

@docs id, position, health, attackDamage, healingPotential, inventory, maxHealth

-}

import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction exposing (Direction)
import Warrior.Internal.Player as Internal
import Warrior.Item exposing (Item)


{-| This record contains the state of a given player. Playable or non-playable.
-}
type alias Player =
    Internal.Player


{-| The different actions which can be performed on a given turn.
-}
type Action
    = Wait
    | Heal
    | Pickup
    | Move Direction
    | Attack Direction


{-| The id of the player
-}
id : Player -> String
id =
    Internal.id


{-| Retrieve the current position of a player.
-}
position : Player -> Coordinate
position =
    Internal.position


{-| Get the health of a player.
-}
health : Player -> Int
health =
    Internal.health


{-| Get the maximum health the player can have. The player usually starts with this amount of health at the start of a map.
-}
maxHealth : Player -> Int
maxHealth =
    Internal.maxHealth


{-| Get all the items that the player has in their posession.
-}
inventory : Player -> List Item
inventory =
    Internal.inventory


{-| Returns how much damage this player will do as the result of an attack. This takes the items in the players inventory into account.
-}
attackDamage : Player -> Int
attackDamage =
    Internal.attackDamage


{-| Returns how many points this player will recover as the result of a heal action. Items in the players inventory will be taken into account.
-}
healingPotential : Player -> Int
healingPotential =
    Internal.healingPotential
