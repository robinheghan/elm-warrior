module Warrior.Coordinate exposing (Coordinate)

{-| Coordinates are simple records that describes a position on a map

@docs Coordinate

-}


{-| A Coordinate is the intersection of column x and row y.
-}
type alias Coordinate =
    { x : Int
    , y : Int
    }
