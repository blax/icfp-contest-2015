module Simulator.Unit where

import Simulator.Cell exposing (..)
import Simulator.Move exposing (..)

type alias Unit =
  { cells: List Cell
  , pivot: Cell
  }

moveUnit move unit =
  { unit |
    cells <- List.map (moveCell move) unit.cells
  , pivot <- (moveCell move) unit.pivot
  }

-- private

isEven : Int -> Bool
isEven n =
  n % 2 == 0
