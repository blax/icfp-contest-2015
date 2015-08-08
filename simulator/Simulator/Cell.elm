module Simulator.Cell where

import Simulator.Move exposing (..)

type alias Cell = (Int, Int)

x : Cell -> Int
x = fst

y : Cell -> Int
y = snd

--moveCellBy (dx, dy) cell =
--  (x cell + dx, y cell + dy)


moveCell move (x, y) =
  case move of
    E  -> (x + 1, y)
    W  -> (x - 1, y)
    SE ->
      if isEven y then
        (x,     y + 1)
      else
        (x + 1, y + 1)
    SW ->
      if isEven y then
        (x - 1, y + 1)
      else
        (x,     y + 1)

-- private

isEven : Int -> Bool
isEven n =
  n % 2 == 0

--bottomEdge : Unit -> Int
--bottomEdge unit =
--  case List.maximum (List.map y unit.cells) of
--    Just y -> y
--    Nothing -> 0
