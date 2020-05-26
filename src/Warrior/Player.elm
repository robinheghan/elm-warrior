module Warrior.Player exposing
    ( Action(..)
    , Item(..)
    , Player
    , PlayerRole(..)
    , addAction
    , addItem
    , alive
    , attack
    , attackDamage
    , currentHealth
    , currentPosition
    , heal
    , inventory
    , maxHealth
    , previousActions
    , spawnHero
    , spawnVillain
    , withPosition
    )

import Warrior.Coordinate exposing (Coordinate)
import Warrior.Direction exposing (Direction)


type Player
    = Player Internals


type PlayerRole
    = Hero
    | Villain


type alias Internals =
    { playerRole : PlayerRole
    , spawnPosition : Coordinate
    , currentPosition : Coordinate
    , previousActions : List Action
    , health : Int
    , maxHealth : Int
    , inventory : List Item
    }


type Action
    = Wait
    | Move Direction
    | Pickup
    | Heal
    | Attack Direction


type Item
    = Sword


spawnHero : Coordinate -> Player
spawnHero cord =
    Player
        { playerRole = Hero
        , spawnPosition = cord
        , currentPosition = cord
        , previousActions = []
        , health = 10
        , maxHealth = 10
        , inventory = []
        }


spawnVillain : Coordinate -> Player
spawnVillain cord =
    Player
        { playerRole = Villain
        , spawnPosition = cord
        , currentPosition = cord
        , previousActions = []
        , health = 10
        , maxHealth = 10
        , inventory = []
        }


currentPosition : Player -> Coordinate
currentPosition (Player fields) =
    fields.currentPosition


currentHealth : Player -> Int
currentHealth (Player fields) =
    fields.health


maxHealth : Player -> Int
maxHealth (Player fields) =
    fields.maxHealth


inventory : Player -> List Item
inventory (Player fields) =
    fields.inventory


addItem : Item -> Player -> Player
addItem item (Player fields) =
    Player { fields | inventory = item :: fields.inventory }


previousActions : Player -> List Action
previousActions (Player fields) =
    fields.previousActions


withPosition : Coordinate -> Player -> Player
withPosition coordinate (Player fields) =
    Player <| { fields | currentPosition = coordinate }


heal : Player -> Player
heal (Player fields) =
    Player { fields | health = min fields.maxHealth (fields.health + 2) }


attack : Player -> Player -> Player
attack attacker (Player defender) =
    Player { defender | health = defender.health - attackDamage attacker }


attackDamage : Player -> Int
attackDamage (Player fields) =
    if List.member Sword fields.inventory then
        4

    else
        1


alive : Player -> Bool
alive (Player fields) =
    fields.health > 0


addAction : Action -> Player -> Player
addAction action (Player fields) =
    Player <| { fields | previousActions = action :: fields.previousActions }
