module Warrior.Direction exposing
    ( Direction(..)
    , all
    )


type Direction
    = Left
    | Right
    | Up
    | Down


all : List Direction
all =
    [ Left
    , Right
    , Up
    , Down
    ]
