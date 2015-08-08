module Simulator.Move where

type Move = E | W | SE | SW

type Rotation = CW | CCW

type Command = Move Move | Rotate Rotation
