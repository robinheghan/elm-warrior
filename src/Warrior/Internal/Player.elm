module Warrior.Internal.Player exposing
    ( Player
    , addItem
    , alive
    , attack
    , attackDamage
    , heal
    , healingPotential
    , health
    , id
    , inventory
    , maxHealth
    , position
    , spawnHero
    , spawnVillain
    , withPosition
    )

import List.Extra as List
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Item as Item exposing (Item)


type Player
    = Player Internals


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


id : Player -> String
id (Player fields) =
    fields.id


position : Player -> Coordinate
position (Player fields) =
    fields.currentPosition


health : Player -> Int
health (Player fields) =
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


withPosition : Coordinate -> Player -> Player
withPosition coordinate (Player fields) =
    Player <| { fields | currentPosition = coordinate }


heal : Player -> Player
heal ((Player fields) as player) =
    Player
        { fields
            | health = min fields.maxHealth (fields.health + healingPotential player)
            , inventory = List.remove Item.Potion fields.inventory
        }


attack : Player -> Player -> Player
attack attacker (Player defender) =
    Player { defender | health = defender.health - attackDamage attacker }


attackDamage : Player -> Int
attackDamage (Player fields) =
    if List.member Item.Sword fields.inventory then
        3

    else
        1


healingPotential : Player -> Int
healingPotential (Player fields) =
    if List.member Item.Potion fields.inventory then
        6

    else
        1


alive : Player -> Bool
alive (Player fields) =
    fields.health > 0
