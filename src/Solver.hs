module Solver where

import InputTypes
import Simulation

import Control.Monad.Logic

lowLevelSolve :: GameState -> [[Command]]
lowLevelSolve state = map snd $ (observeMany 20) $ findLockedPositions state [] []

findLockedPositions :: GameState -> [Command] -> [Unit] -> Logic (GameState, [Command])
findLockedPositions state cs visited
  | gLockedState state = return (state, cs)
  | elem (gCurrentUnit state) visited = mzero
  | otherwise = do
      next <- msum . map return $ validMoves
      findLockedPositions (applyCommand state next) (next:cs) ((gCurrentUnit state):visited)

validMoves :: [Command]
validMoves = [Move SW, Move SE, Move E, Move W, Rotate CW, Rotate CCW]

heuristicScore :: GameState -> Int
heuristicScore state =
  minimum $ map cY $ gFilled state
--
