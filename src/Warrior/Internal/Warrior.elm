module Warrior.Internal.Warrior exposing
    ( Warrior
    , addItem
    , alive
    , attack
    , attackDamage
    , heal
    , healingPotential
    , health
    , id
    , inventory
    , isHero
    , isVillain
    , maxHealth
    , position
    , spawnHero
    , spawnVillain
    , withPosition
    )

import List.Extra as List
import Warrior.Coordinate exposing (Coordinate)
import Warrior.Item as Item exposing (Item)


type Warrior
    = Warrior Internals


type Role
    = Hero
    | Villain


type alias Internals =
    { id : String
    , role : Role
    , spawnPosition : Coordinate
    , currentPosition : Coordinate
    , health : Int
    , maxHealth : Int
    , inventory : List Item
    }


spawnHero : String -> Coordinate -> Warrior
spawnHero id_ cord =
    Warrior
        { id = id_
        , role = Hero
        , spawnPosition = cord
        , currentPosition = cord
        , health = 10
        , maxHealth = 10
        , inventory = []
        }


spawnVillain : String -> Coordinate -> Warrior
spawnVillain id_ cord =
    Warrior
        { id = id_
        , role = Villain
        , spawnPosition = cord
        , currentPosition = cord
        , health = 10
        , maxHealth = 10
        , inventory = []
        }


id : Warrior -> String
id (Warrior fields) =
    fields.id


position : Warrior -> Coordinate
position (Warrior fields) =
    fields.currentPosition


health : Warrior -> Int
health (Warrior fields) =
    fields.health


maxHealth : Warrior -> Int
maxHealth (Warrior fields) =
    fields.maxHealth


inventory : Warrior -> List Item
inventory (Warrior fields) =
    fields.inventory


addItem : Item -> Warrior -> Warrior
addItem item (Warrior fields) =
    Warrior { fields | inventory = item :: fields.inventory }


withPosition : Coordinate -> Warrior -> Warrior
withPosition coordinate (Warrior fields) =
    Warrior <| { fields | currentPosition = coordinate }


heal : Warrior -> Warrior
heal ((Warrior fields) as player) =
    Warrior
        { fields
            | health = min fields.maxHealth (fields.health + healingPotential player)
            , inventory = List.remove Item.Potion fields.inventory
        }


attack : Warrior -> Warrior -> Warrior
attack attacker (Warrior defender) =
    Warrior { defender | health = defender.health - attackDamage attacker }


attackDamage : Warrior -> Int
attackDamage (Warrior fields) =
    if List.member Item.Sword fields.inventory then
        3

    else
        1


healingPotential : Warrior -> Int
healingPotential (Warrior fields) =
    if List.member Item.Potion fields.inventory then
        6

    else
        1


alive : Warrior -> Bool
alive (Warrior fields) =
    fields.health > 0


isHero : Warrior -> Bool
isHero (Warrior fields) =
    fields.role == Hero


isVillain : Warrior -> Bool
isVillain (Warrior fields) =
    fields.role == Villain
