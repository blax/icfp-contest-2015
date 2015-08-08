module Simulator.Cell where

import Simulator.Command exposing (..)

type alias Cell = (Int, Int)

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

-- private

isEven : Int -> Bool
isEven n =
  n % 2 == 0
