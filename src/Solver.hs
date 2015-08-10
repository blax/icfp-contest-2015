module Solver where

import InputTypes
import Simulation

import Data.Ord
import Data.List

solve :: GameState -> [Command] -> ([Command], GameState)
solve state commands = (reverse bestCommands, bestState) -- bestState {gHeight = length sorted})
  where
    (bestCommands, bestState) = head sorted
    sorted = sortWith bestScore $ traverseHighLevelGraph [(commands,state)] [] []
    bestScore = (\(cs, st) -> 0 - gScore st)

traverseHighLevelGraph :: [([Command], GameState)] -> [GameState] -> [([Command], GameState)] -> [([Command], GameState)]
traverseHighLevelGraph [] _ solutions = solutions
traverseHighLevelGraph ((path,s):queue) visited solutions
  | elem s visited = traverseHighLevelGraph queue visited solutions
  | gGameOver s = traverseHighLevelGraph queue (s:visited) ((path,s):solutions)
  | otherwise = traverseHighLevelGraph (neighbors++queue) (s:visited) solutions
  where
    neighbors = filter validNeighbor $ take 1 $ map (\(a,b,c)-> (a++path,c)) $ solveLowLevel s
    validNeighbor (_, n) = (not $ elem n visited)

solveLowLevel :: GameState -> [([Command], Int, GameState)]
solveLowLevel state = sortedCandidates
  where
    sortedCandidates = sortWith (\(_,b,_) -> 0-b) ratedCandidates
    ratedCandidates = map (\(cs, s) -> (cs, heuristicScore s, s)) uniqueCandidates
    uniqueCandidates = nubBy (\(_,a) (_,b) -> a==b) candidates
    candidates = findLockedPositions [([],state {gLockedState = False})] [] []

findLockedPositions :: [([Command], GameState)] -> [GameState] -> [([Command], GameState)] -> [([Command], GameState)]
findLockedPositions [] _ solutions = solutions
findLockedPositions ((path,s):queue) visited solutions
  | gLockedState s = findLockedPositions queue (s:visited) ((path,s):solutions)
  | gGameOver s = findLockedPositions queue (s:visited) ((path,s):solutions)
  | otherwise = findLockedPositions (neighbors++queue) (s:visited) solutions
  where
    neighbors = filter validNeighbor $ map (\m -> ((m:path), applyCommand s m)) validMoves
    validNeighbor (_,n) =
      (not $ elem n visited)

validMoves :: [Command]
validMoves = [Move SW, Move SE, Move E, Move W, Rotate CW, Rotate CCW]

heuristicScore :: GameState -> Int
heuristicScore state = 3 * linesCleared + 3 * aggHeight - 2 * holes + (gScore state)
  where
    bumpiness = snd $ foldl' (\(prev,bumpSum) col -> (col, bumpSum + ((abs (col-prev))`div` 1))) (head heights, 0) (tail heights)
    aggHeight = (sum $ heights)
    heights = map (minimum . map snd) $ grouped
    linesCleared = gPrevLinesCleared state
    grouped = (groupBy sameX $ fields)
    fields = sortWith fst $ map unskewedCoords $ gFilled state
    sameX (a,_) (b,_) = a == b

    holes = length $ filter isHole $ [(x-(y `quot` 2),y) | x <- [0..(-1 + gWidth state)], y <- [0..(-1 + gHeight state)]]
    isHole (x,y) = (not . elem (Cell x y) $ gFilled state) && (all (\c -> elem c $ gFilled state) $ surrounding (x,y))
    surrounding (x,y) = map (\(xd, yd) -> Cell (x+xd) (y+yd)) [(0,-1),(1,-1)]
