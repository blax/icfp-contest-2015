module Simulator.Cell where

import Simulator.Command exposing (..)

type alias Cell = (Int, Int)

-- InputCells are cells in the coordinate system defined in the task.
-- Regular cells use the coordinates with non-perpendicular axes.

type InputCell = InputCell (Int, Int)

x : Cell -> Int
x = fst

y : Cell -> Int
y = snd

moveCell : Move -> Cell -> Cell
moveCell move (x, y) =
  case move of
    E  -> (x + 1, y)
    W  -> (x - 1, y)
    SE -> (x, y + 1)
    SW -> (x - 1, y + 1)

rotateCell : Rotation -> Cell -> Cell -> Cell
rotateCell rotation pivot cell =
  let
    (cx, cy) = cell
    (px, py) = pivot
    (dx, dy) = (cx - px, cy - py)
    (dx', dy') =
      case rotation of
        CW ->
          (-1 * dy, dx + dy)
        CCW ->
          (dx + dy, -1 * dx)
  in
    (dx' + px, dy' + py)

fromInputCell : InputCell -> Cell
fromInputCell (InputCell (x, y)) =
  (x - floor (toFloat y / 2), y)

toInputCell : Cell -> InputCell
toInputCell (x, y) =
  InputCell (x + floor (toFloat y / 2), y)

toInputCellCoords cell =
  let unpack = \(InputCell coords) -> coords
  in
    unpack (toInputCell cell)
