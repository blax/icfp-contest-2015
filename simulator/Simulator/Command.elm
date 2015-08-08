module Simulator.Command where

type Move = E | W | SE | SW

type Rotation = CW | CCW

type Command = Move Move | Rotate Rotation

type alias CommandCode = Char
