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

fromInputCellsUnit inputUnit =
  { inputUnit |
    cells <- (List.map fromInputCell inputUnit.cells)
  , pivot <- fromInputCell inputUnit.pivot
  }

centerUnit : Int -> Unit -> Unit
centerUnit boardWidth unit =
  let
    distance = (boardWidth - (unitWidth unit)) // 2 - (fst << edgesOfUnit) unit
    command  = (if distance < 0 then Move W else Move E)
    commands = List.repeat (abs distance) command
  in
    List.foldl commandUnit unit commands

edgesOfUnit : Unit -> (Int, Int)
edgesOfUnit unit =
  let
    inputCells =
      List.map toInputCell unit.cells

    toCoords =
      \(InputCell coords) -> coords

    xs =
      (List.map (fst << toCoords)) inputCells

    ((Just min), (Just max)) = (List.minimum xs, List.maximum xs)
  in
    (min, max)

unitWidth : Unit -> Int
unitWidth unit =
  let
    (left, right) = edgesOfUnit unit
  in
    right - left + 1
