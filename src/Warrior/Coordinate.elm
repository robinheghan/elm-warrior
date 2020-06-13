module Warrior.Coordinate exposing
    ( Coordinate
    , toString
    )

{-| Coordinates are simple records that describes a position on a map

@docs Coordinate

@docs toString

-}


{-| A Coordinate is the intersection of column x and row y.
-}
type alias Coordinate =
    { x : Int
    , y : Int
    }


{-| A human readable string of the given coordinate.
-}
toString : Coordinate -> String
toString cord =
    String.join ""
        [ "( "
        , String.fromInt cord.x
        , ", "
        , String.fromInt cord.y
        , " )"
        ]
