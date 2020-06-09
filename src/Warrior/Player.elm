module Warrior.Player exposing
    ( Player, PlayerRole(..), Action(..)
    , spawnHero, spawnVillain, addItem, withPosition, attack, heal
    , id, position, health, alive, attackDamage, healingPotential, inventory, maxHealth
    )

{-| The player record contains all known state for playable, as well as non-playable characters in the game. You'll probably need the functions in this module to make decisions about your next turn.

@docs Player, PlayerRole, Action

@docs spawnHero, spawnVillain, addItem, withPosition, attack, heal

@docs id, position, health, alive, attackDamage, healingPotential, inventory, maxHealth

-}

import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction exposing (Direction)
import Warrior.Item as Item exposing (Item)


{-| This record contains the state of a given player. Playable or non-playable.
-}
type Player
    = Player Internals


{-| This type allows us to seperate playable from non-playable characters. Villains are players which belong to a specific map, where they serve as obstacles.
-}
type PlayerRole
    = Hero
    | Villain


type alias Internals =
    { id : String
    , playerRole : PlayerRole
    , spawnPosition : Coordinate
    , currentPosition : Coordinate
    , health : Int
    , maxHealth : Int
    , inventory : List Item
    }


{-| The different actions which can be performed on a given turn.
-}
type Action
    = Wait
    | Move Direction
    | Pickup
    | Heal
    | Attack Direction


{-| Initializes a hero. You do not need to concern yourself with this function, as it will be called by the framework.
-}
spawnHero : String -> Coordinate -> Player
spawnHero id_ cord =
    Player
        { id = id_
        , playerRole = Hero
        , spawnPosition = cord
        , currentPosition = cord
        , health = 10
        , maxHealth = 10
        , inventory = []
        }


{-| Initializes a villain. You do not need to concern yourself with this function, as it will be called by the framework.
-}
spawnVillain : String -> Coordinate -> Player
spawnVillain id_ cord =
    Player
        { id = id_
        , playerRole = Villain
        , spawnPosition = cord
        , currentPosition = cord
        , health = 10
        , maxHealth = 10
        , inventory = []
        }


{-| The id of the player
-}
id : Player -> String
id (Player fields) =
    fields.id


{-| Retrieve the current position of a player.
-}
position : Player -> Coordinate
position (Player fields) =
    fields.currentPosition


{-| Get the health of a player.
-}
health : Player -> Int
health (Player fields) =
    fields.health


{-| Get the maximum health the player can have. The player usually starts with this amount of health at the start of a map.
-}
maxHealth : Player -> Int
maxHealth (Player fields) =
    fields.maxHealth


{-| Get all the items that the player has in their posession.
-}
inventory : Player -> List Item
inventory (Player fields) =
    fields.inventory


{-| Add an item to the players inventory. You do not need to concern yourself with this function, as the framework will use this as the result of a Pickup action.
-}
addItem : Item -> Player -> Player
addItem item (Player fields) =
    Player { fields | inventory = item :: fields.inventory }


{-| Change the current position of a player. This will be used by the framework to advance the state of the game. You do not need to concern yourself with this function.
-}
withPosition : Coordinate -> Player -> Player
withPosition coordinate (Player fields) =
    Player <| { fields | currentPosition = coordinate }


{-| This function will be called by the framework as a result of the Heal action, and can be safely ignored.
-}
heal : Player -> Player
heal ((Player fields) as player) =
    Player
        { fields
            | health = min fields.maxHealth (fields.health + healingPotential player)
            , inventory = List.filter ((/=) Item.Potion) fields.inventory
        }


{-| This function will be called by the framework as a result of the Attack action, and can be safely ignored.
-}
attack : Player -> Player -> Player
attack attacker (Player defender) =
    Player { defender | health = defender.health - attackDamage attacker }


{-| Returns how much damage this player will do as the result of an attack. This takes the items in the players inventory into account.
-}
attackDamage : Player -> Int
attackDamage (Player fields) =
    if List.member Item.Sword fields.inventory then
        3

    else
        1


{-| Returns how many points this player will recover as the result of a heal action. Items in the players inventory will be taken into account.
-}
healingPotential : Player -> Int
healingPotential (Player fields) =
    if List.member Item.Potion fields.inventory then
        6

    else
        1


{-| Checks if the player is alive.
-}
alive : Player -> Bool
alive (Player fields) =
    fields.health > 0
