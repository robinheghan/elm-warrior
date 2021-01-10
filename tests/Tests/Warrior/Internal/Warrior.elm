module Tests.Warrior.Internal.Warrior exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Warrior.Internal.Warrior as Player
import Warrior.Item as Item


all : Test
all =
    describe "Internal.Warrior"
        [ describe "spawnHero" spawnHeroTests
        , describe "spawnVillain" spawnVillainTests
        , describe "addItem" addItemTests
        , describe "withPosition" withPositionTests
        , describe "attack" attackTests
        , describe "attackDamage" attackDamageTests
        , describe "heal" healTests
        , describe "healingPotential" healingPotentialTests
        , describe "alive" aliveTests
        ]


initialHealth : Int
initialHealth =
    10


spawnHeroTests : List Test
spawnHeroTests =
    [ test "returns a Hero with the given id and coords" <|
        \() ->
            let
                name =
                    "at"

                coords =
                    { x = 0, y = 0 }
            in
            Player.spawnHero name coords
                |> Expect.all
                    [ Player.id >> Expect.equal name
                    , Player.position >> Expect.equal coords
                    , Player.isHero >> Expect.equal True
                    , Player.isVillain >> Expect.equal False

                    -- Defaults
                    , Player.health >> Expect.equal initialHealth
                    , Player.maxHealth >> Expect.equal initialHealth
                    , Player.inventory >> List.isEmpty >> Expect.equal True
                    ]
    ]


spawnVillainTests : List Test
spawnVillainTests =
    [ test "returns a Villain with the given id and coords" <|
        \() ->
            let
                name =
                    "evil"

                coords =
                    { x = 1, y = 1 }
            in
            Player.spawnVillain name coords
                |> Expect.all
                    [ Player.id >> Expect.equal name
                    , Player.position >> Expect.equal coords
                    , Player.isVillain >> Expect.equal True
                    , Player.isHero >> Expect.equal False

                    -- Defaults
                    , Player.health >> Expect.equal initialHealth
                    , Player.maxHealth >> Expect.equal initialHealth
                    , Player.inventory >> List.isEmpty >> Expect.equal True
                    ]
    ]


addItemTests : List Test
addItemTests =
    [ test "adds an item to the inventory" <|
        \() ->
            testPlayer
                |> Player.addItem Item.Sword
                |> Player.addItem Item.Potion
                |> Player.inventory
                |> Expect.equalLists [ Item.Potion, Item.Sword ]
    ]


withPositionTests : List Test
withPositionTests =
    [ test "updates the player's position" <|
        \() ->
            let
                coords =
                    { x = 1, y = 2 }
            in
            testPlayer
                |> Player.withPosition coords
                |> Player.position
                |> Expect.equal coords
    ]


attackTests : List Test
attackTests =
    [ test "with no Sword damages the defender by 1" <|
        \() ->
            let
                attacker =
                    testPlayer
            in
            testPlayer
                |> Player.attack attacker
                |> Player.health
                |> Expect.equal 9
    , test "with a Sword damages the defender by 3" <|
        \() ->
            let
                attacker =
                    testPlayer
                        |> Player.addItem Item.Sword
            in
            testPlayer
                |> Player.attack attacker
                |> Player.health
                |> Expect.equal 7
    , test "can damage a player to below 0 health" <|
        \() ->
            let
                attacker =
                    testPlayer
                        |> Player.addItem Item.Sword
            in
            playerWithHealth 1
                |> Player.attack attacker
                |> Player.health
                |> Expect.equal -2
    ]


attackDamageTests : List Test
attackDamageTests =
    [ test "with no Sword damage is 1" <|
        \() ->
            testPlayer
                |> Player.attackDamage
                |> Expect.equal 1
    , test "with a Sword damage is 3" <|
        \() ->
            testPlayer
                |> Player.addItem Item.Sword
                |> Player.attackDamage
                |> Expect.equal 3
    , test "with 2 Swords damage is still 3" <|
        \() ->
            testPlayer
                |> Player.addItem Item.Sword
                |> Player.addItem Item.Sword
                |> Player.attackDamage
                |> Expect.equal 3
    ]


healTests : List Test
healTests =
    [ test "with 1 health and no potion heals by 1" <|
        \() ->
            playerWithHealth 1
                |> Player.heal
                |> Player.health
                |> Expect.equal 2
    , test "healing does not pass max health (10)" <|
        \() ->
            testPlayer
                |> Player.heal
                |> Player.health
                |> Expect.equal 10
    , test "with a potion, uses potion and heals by 6" <|
        \() ->
            playerWithHealth 1
                |> Player.addItem Item.Potion
                |> Player.heal
                |> Expect.all
                    [ Player.health >> Expect.equal 7
                    , Player.inventory >> Expect.equalLists []
                    ]
    , test "with 2 potions, uses 1 potion and heals by 6" <|
        \() ->
            playerWithHealth 1
                |> Player.addItem Item.Potion
                |> Player.addItem Item.Potion
                |> Player.heal
                |> Expect.all
                    [ Player.health >> Expect.equal 7
                    , Player.inventory >> Expect.equalLists [ Item.Potion ]
                    ]
    , test "with high health and a potion, uses potion to heal to full" <|
        \() ->
            playerWithHealth 8
                |> Player.addItem Item.Potion
                |> Player.heal
                |> Expect.all
                    [ Player.health >> Expect.equal 10
                    , Player.inventory >> Expect.equalLists []
                    ]
    , test "with full health and a potion, uses the potion" <|
        \() ->
            testPlayer
                |> Player.addItem Item.Potion
                |> Player.heal
                |> Expect.all
                    [ Player.health >> Expect.equal 10
                    , Player.inventory >> Expect.equalLists []
                    ]
    ]


healingPotentialTests : List Test
healingPotentialTests =
    [ test "with no potions is 1" <|
        \() ->
            testPlayer
                |> Player.healingPotential
                |> Expect.equal 1
    , test "with a potion is 6" <|
        \() ->
            testPlayer
                |> Player.addItem Item.Potion
                |> Player.healingPotential
                |> Expect.equal 6
    , test "with many potions is 6" <|
        \() ->
            testPlayer
                |> Player.addItem Item.Potion
                |> Player.addItem Item.Potion
                |> Player.addItem Item.Potion
                |> Player.healingPotential
                |> Expect.equal 6
    ]


aliveTests : List Test
aliveTests =
    [ test "with full health is alive" <|
        \() ->
            testPlayer
                |> Player.alive
                |> Expect.equal True
    , test "with 1 health is alive" <|
        \() ->
            playerWithHealth 1
                |> Player.alive
                |> Expect.equal True
    , test "with 0 health is not alive" <|
        \() ->
            playerWithHealth 0
                |> Player.alive
                |> Expect.equal False
    , test "with health below 0 is not alive" <|
        \() ->
            playerWithHealth -1
                |> Player.alive
                |> Expect.equal False
    ]


testPlayer : Player.Warrior
testPlayer =
    Player.spawnHero "test" { x = 0, y = 0 }


playerWithHealth : Int -> Player.Warrior
playerWithHealth health =
    testPlayer
        |> injurePlayerUntilHealthIs health


injurePlayerUntilHealthIs : Int -> Player.Warrior -> Player.Warrior
injurePlayerUntilHealthIs health player =
    if Player.health player > health then
        injurePlayerUntilHealthIs health (Player.attack testPlayer player)

    else
        player
