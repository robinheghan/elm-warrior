module Warrior.Coordinate exposing
    ( Coordinate
    , toString
    )

{-| Coordinates are simple records that describes a position on a map

@docs Coordinate

-}


{-| A Coordinate is the intersection of column x and row y.
-}
type alias Coordinate =
    { x : Int
    , y : Int
    }


toString : Coordinate -> String
toString cord =
    String.join ""
        [ "( "
        , String.fromInt cord.x
        , ", "
        , String.fromInt cord.y
        , " )"
        ]
