module Warrior exposing
    ( Warrior, Action(..)
    , id, position, health, maxHealth, healingPotential, attackDamage, inventory
    )

{-| You'll need the functions and Action type in this module to implement a turn function. The Map module can also be of some assistance.

@docs Warrior, Action

@docs id, position, health, maxHealth, healingPotential, attackDamage, inventory

-}

import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction exposing (Direction)
import Warrior.Internal.Warrior as Internal
import Warrior.Item exposing (Item)


{-| This type represents a warrior.
-}
type alias Warrior =
    Internal.Warrior


{-| The different actions which can be performed on a given turn.
_Wait_ will simply skip your turn.
_Heal_ let's the warrior recover some health. How much is influenced by the existence of potions in your inventory.
_Pickup_ will pick an item from the tile you are on, and place it on your inventory.
_Move_ let's you move one tile in any direction.
_Attack_ will make you attack in any direction. If another warrior is there that warrior will lose health. How much is influenced by the existance of a sword in your inventory.
-}
type Action
    = Wait
    | Heal
    | Pickup
    | Move Direction
    | Attack Direction


{-| The id, or name, of the warrior.
-}
id : Warrior -> String
id =
    Internal.id


{-| Retrieve the current position of a warrior.
-}
position : Warrior -> Coordinate
position =
    Internal.position


{-| Get the health of a warrior.
-}
health : Warrior -> Int
health =
    Internal.health


{-| Get the maximum health the warrior can have. The warrior will start a map with this much health.
-}
maxHealth : Warrior -> Int
maxHealth =
    Internal.maxHealth


{-| A list of all the items a warrior has in its possession.
-}
inventory : Warrior -> List Item
inventory =
    Internal.inventory


{-| Calculates how much damage the warrior will do as the result of an attack. The returned value depends on the items in the inventory.
-}
attackDamage : Warrior -> Int
attackDamage =
    Internal.attackDamage


{-| Calculates much health the warrior could _potentially_ recover as the result of a heal action. This function doesn't consider your current health; you will never heal past your `maxHealth`.
-}
healingPotential : Warrior -> Int
healingPotential =
    Internal.healingPotential
