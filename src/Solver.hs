module Solver where

import InputTypes
import Simulation

import Data.Ord
import Data.List

solve :: GameState -> [([Command], Int, GameState)]
solve state = sortedCandidates
  where
    sortedCandidates = sortWith (\(_,b,_) -> b) ratedCandidates
    ratedCandidates = map (\(cs, s) -> (cs, heuristicScore s, s)) candidates
    candidates = findLockedPositions [([],state)] [] []

findLockedPositions :: [([Command], GameState)] -> [Unit] -> [([Command], GameState)] -> [([Command], GameState)]
findLockedPositions [] _ solutions = solutions
findLockedPositions ((path,s):queue) visited solutions =
    findLockedPositions (neighbors++queue) (unit:visited) newSolutions
  where
    neighbors = filter validNeighbor $ map (\m -> ((m:path), applyCommand s m)) validMoves
    validNeighbor (_,n) = not $ elem (gCurrentUnit n) visited
    newSolutions
      | gLockedState s = ((reverse path, s):solutions)
      | otherwise = solutions
    unit = gCurrentUnit s

validMoves :: [Command]
validMoves = reverse [Move SW, Move SE, Move E, Move W, Rotate CW, Rotate CCW]

heuristicScore :: GameState -> Int
heuristicScore state =
  100 - (minimum $ map cY $ gFilled state)

sortWith :: Ord b => (a -> b) -> [a] -> [a]
sortWith f = map fst . sortBy (comparing snd) . map (\x -> (x, f x))
