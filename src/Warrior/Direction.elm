module Warrior.Direction exposing
    ( Direction(..)
    , all
    )

{-| Directions are usually used to specify where an action is applied.

@docs Direction
@docs all

-}


{-| The different directions a player can move, look and attack. Note that there are no diagonal directions.
-}
type Direction
    = Left
    | Right
    | Up
    | Down


{-| A simple list containing all Direction values. Handy when combined with `List.filter` or `List.map`
-}
all : List Direction
all =
    [ Left
    , Right
    , Up
    , Down
    ]
