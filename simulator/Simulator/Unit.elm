module Simulator.Unit where

import Simulator.Cell exposing (..)
import Simulator.Command exposing (..)

type alias Unit =
  { cells: List Cell
  , pivot: Cell
  }

moveUnit : Move -> Unit -> Unit
moveUnit move unit =
  { unit |
    cells <- List.map (moveCell move) unit.cells
  , pivot <- (moveCell move) unit.pivot
  }

rotateUnit : Rotation -> Unit -> Unit
rotateUnit rotation unit =
  { unit |
    cells <- List.map (rotateCell rotation unit.pivot) unit.cells
  }

commandUnit : Command -> Unit -> Unit
commandUnit command =
  case command of
    Move m -> moveUnit m
    Rotate r -> rotateUnit r
