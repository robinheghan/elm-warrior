module Warrior.Maps exposing
    ( all, movement, fighting
    , straight, inverseL, loop, withDeadEndLeft, withDeadEndRight, openSpace, openSpaceReverse
    , straightGuard, straightPowerfulGuard, straightGuardPickupSword, straightGuardPickupPotion, dungeon
    )

{-| A selection of pre-made maps to test your warrior.

@docs all, movement, fighting


# Movement only

@docs straight, inverseL, loop, withDeadEndLeft, withDeadEndRight, openSpace, openSpaceReverse


# Fighing only

@docs straightGuard, straightPowerfulGuard, straightGuardPickupSword, straightGuardPickupPotion, dungeon

-}

import Warrior.Item as Item
import Warrior.Map exposing (Map)
import Warrior.Map.Builder as Map
import Warrior.Npc.Dummy as Dummy
import Warrior.Npc.StationaryAttacker as StationaryAttacker


{-| A list of all pre-made maps.
-}
all : List Map
all =
    List.concat
        [ movement
        , fighting
        ]


{-| A list of map that only require navigation.
-}
movement : List Map
movement =
    [ straight
    , inverseL
    , loop
    , withDeadEndLeft
    , withDeadEndRight
    , openSpace
    , openSpaceReverse
    ]


{-| A list of maps which require fighting in addition to navigation.
-}
fighting : List Map
fighting =
    [ straightGuard
    , straightPowerfulGuard
    , straightGuardPickupSword
    , straightGuardPickupPotion
    , dungeon
    ]



-- MOVEMENT MAPS


{-| -}
straight : Map
straight =
    Map.init { rows = 1, columns = 5 }
        |> Map.withDescription "The goal for these first couple of maps is simply to reach that green area, also known as the exit point. Let's see if we can reach it."
        |> Map.withSpawnPoint { x = 0, y = 0 }
        |> Map.withExitPoint { x = 4, y = 0 }
        |> Map.build


{-| -}
inverseL : Map
inverseL =
    Map.init { rows = 5, columns = 5 }
        |> Map.withWalledArea { x = 0, y = 1 } { x = 3, y = 4 }
        |> Map.withSpawnPoint { x = 0, y = 0 }
        |> Map.withExitPoint { x = 4, y = 4 }
        |> Map.build


{-| -}
loop : Map
loop =
    Map.init { rows = 7, columns = 7 }
        |> Map.withSpawnPoint { x = 0, y = 0 }
        |> Map.withExitPoint { x = 3, y = 3 }
        |> Map.withWalledArea { x = 0, y = 1 } { x = 5, y = 1 }
        |> Map.withWalledArea { x = 5, y = 1 } { x = 5, y = 5 }
        |> Map.withWalledArea { x = 5, y = 5 } { x = 1, y = 5 }
        |> Map.withWalledArea { x = 1, y = 5 } { x = 1, y = 3 }
        |> Map.build


{-| -}
withDeadEndLeft : Map
withDeadEndLeft =
    Map.init { rows = 7, columns = 7 }
        |> Map.withSpawnPoint { x = 3, y = 0 }
        |> Map.withExitPoint { x = 6, y = 1 }
        |> Map.withWalledArea { x = 0, y = 0 } { x = 1, y = 0 }
        |> Map.withWalledArea { x = 5, y = 0 } { x = 6, y = 0 }
        |> Map.withWalledArea { x = 0, y = 1 } { x = 2, y = 2 }
        |> Map.withWalledArea { x = 1, y = 4 } { x = 2, y = 6 }
        |> Map.withWalledArea { x = 4, y = 1 } { x = 5, y = 2 }
        |> Map.withWalledArea { x = 4, y = 4 } { x = 6, y = 6 }
        |> Map.withWalledArea { x = 0, y = 6 } { x = 0, y = 6 }
        |> Map.build


{-| -}
withDeadEndRight : Map
withDeadEndRight =
    Map.init { rows = 7, columns = 7 }
        |> Map.withSpawnPoint { x = 3, y = 0 }
        |> Map.withExitPoint { x = 0, y = 5 }
        |> Map.withWalledArea { x = 0, y = 0 } { x = 1, y = 0 }
        |> Map.withWalledArea { x = 5, y = 0 } { x = 6, y = 0 }
        |> Map.withWalledArea { x = 0, y = 1 } { x = 2, y = 2 }
        |> Map.withWalledArea { x = 1, y = 4 } { x = 2, y = 6 }
        |> Map.withWalledArea { x = 4, y = 1 } { x = 5, y = 2 }
        |> Map.withWalledArea { x = 4, y = 4 } { x = 6, y = 6 }
        |> Map.withWalledArea { x = 0, y = 6 } { x = 0, y = 6 }
        |> Map.build


{-| -}
openSpace : Map
openSpace =
    Map.init { rows = 7, columns = 7 }
        |> Map.withSpawnPoint { x = 1, y = 1 }
        |> Map.withExitPoint { x = 5, y = 5 }
        |> Map.build


{-| -}
openSpaceReverse : Map
openSpaceReverse =
    Map.init { rows = 7, columns = 7 }
        |> Map.withSpawnPoint { x = 5, y = 5 }
        |> Map.withExitPoint { x = 1, y = 1 }
        |> Map.build



-- FIGHTING MAPS


{-| -}
straightGuard : Map
straightGuard =
    Map.init { rows = 1, columns = 7 }
        |> Map.withDescription "Hmm... Another character. Too bad she's in our way. Attack!"
        |> Map.withSpawnPoint { x = 0, y = 0 }
        |> Map.withExitPoint { x = 6, y = 0 }
        |> Map.withNpc "Dummy" { x = 3, y = 0 } Dummy.takeTurn
        |> Map.build


{-| -}
straightPowerfulGuard : Map
straightPowerfulGuard =
    Map.init { rows = 1, columns = 7 }
        |> Map.withDescription "Be careful, this one might hit back. Be ready to heal if your health becomes low."
        |> Map.withSpawnPoint { x = 0, y = 0 }
        |> Map.withExitPoint { x = 6, y = 0 }
        |> Map.withNpc "Guard" { x = 3, y = 0 } StationaryAttacker.takeTurn
        |> Map.armLastNpc Item.Sword
        |> Map.build


{-| -}
straightGuardPickupSword : Map
straightGuardPickupSword =
    Map.init { rows = 1, columns = 7 }
        |> Map.withDescription "See that yellow tile? It's an item. Let's pick it up, it might improve our attacks."
        |> Map.withSpawnPoint { x = 0, y = 0 }
        |> Map.withExitPoint { x = 6, y = 0 }
        |> Map.withNpc "Guard" { x = 4, y = 0 } StationaryAttacker.takeTurn
        |> Map.withItem { x = 2, y = 0 } Item.Sword
        |> Map.build


{-| -}
straightGuardPickupPotion : Map
straightGuardPickupPotion =
    Map.init { rows = 1, columns = 7 }
        |> Map.withDescription "Another item? Looks potion'y like. Wonder what it will do..."
        |> Map.withSpawnPoint { x = 0, y = 0 }
        |> Map.withExitPoint { x = 6, y = 0 }
        |> Map.withNpc "Guard" { x = 4, y = 0 } StationaryAttacker.takeTurn
        |> Map.armLastNpc Item.Sword
        |> Map.withItem { x = 2, y = 0 } Item.Potion
        |> Map.build


{-| -}
dungeon : Map
dungeon =
    Map.init { rows = 7, columns = 7 }
        |> Map.withDescription "This is the true test. Make it out alive."
        |> Map.withSpawnPoint { x = 0, y = 6 }
        |> Map.withExitPoint { x = 3, y = 0 }
        |> Map.withWalledArea { x = 0, y = 0 } { x = 2, y = 1 }
        |> Map.withWalledArea { x = 4, y = 0 } { x = 6, y = 1 }
        |> Map.withWalledArea { x = 0, y = 5 } { x = 5, y = 5 }
        |> Map.withWalledArea { x = 6, y = 3 } { x = 1, y = 3 }
        |> Map.withNpc "Guard 1" { x = 6, y = 5 } StationaryAttacker.takeTurn
        |> Map.withNpc "Guard 2" { x = 0, y = 3 } StationaryAttacker.takeTurn
        |> Map.withNpc "Boss" { x = 3, y = 1 } StationaryAttacker.takeTurn
        |> Map.armLastNpc Item.Sword
        |> Map.withItem { x = 6, y = 4 } Item.Sword
        |> Map.withItem { x = 0, y = 2 } Item.Potion
        |> Map.build
