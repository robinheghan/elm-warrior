module Warrior.Item exposing
    ( Item(..)
    , toString
    )

{-| A warrior might find and use different items on their quest.

@docs Item
@docs toString

-}


{-| The different items that can be encountered and used during the game.
-}
type Item
    = Sword


{-| Human readable name of item
-}
toString : Item -> String
toString item =
    case item of
        Sword ->
            "Sword"
