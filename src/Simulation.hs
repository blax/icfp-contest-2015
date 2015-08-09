module Simulation where

import InputTypes
import LCGen (mkLCGen, randoms)

data MoveCommand = E | W | SE | SW
data RotationCommand = CW | CCW
data Command = Move MoveCommand | Rotate RotationCommand

data GameState = GameState { gCurrentUnit   :: Unit
                           , gNextUnits     :: [Unit]
                           , gFilled        :: [Cell]
                           , gWidth         :: Int
                           , gHeight        :: Int
                           }
                 deriving Show

initialState :: Input -> Int -> GameState
initialState i seed =
    GameState { gCurrentUnit = currentUnit
              , gNextUnits = nextUnits
              , gFilled = iFilled i
              , gWidth = iWidth i
              , gHeight = iHeight i
              }
  where
    (currentUnit:nextUnits) = map getUnit unitIndexes
    getUnit n = units !! (mod n $ length units)
    unitIndexes = take (iSourceLength i) $ randoms $ mkLCGen seed
    units = map (centerUnit $ iWidth i) (iUnits i)

applyCommand :: GameState -> Command -> GameState
applyCommand state command = state { gCurrentUnit = newCurrentUnit }
  where
    newCurrentUnit = applyCommandToUnit currentUnit command
    currentUnit = gCurrentUnit state

applyCommandToUnit :: Unit -> Command -> Unit
applyCommandToUnit unit (Move direction) =
    unit { uMembers = newMembers, uPivot = newPivot }
  where
    newMembers = map (moveCell direction) $ uMembers unit
    newPivot = moveCell direction $ uPivot unit
applyCommandToUnit unit (Rotate direction) =
    unit { uMembers = newMembers }
  where
    newMembers = map (rotateCell pivot direction) $ uMembers unit
    pivot = uPivot unit

moveCell :: MoveCommand -> Cell -> Cell
moveCell move cell =
    moveCellBy dX dY cell
  where
    (dX, dY) = case move of
      E  -> (1, 0)
      W  -> (-1, 0)
      SE -> (0, 1)
      SW -> (-1, 1)

moveCellBy :: Int -> Int -> Cell -> Cell
moveCellBy dX dY Cell { cX = x, cY = y } =
  Cell { cX = (x+dX), cY = (y+dY) }

rotateCell :: Cell -> RotationCommand -> Cell -> Cell
rotateCell pivot direction Cell { cX = x, cY = y } =
    Cell { cX = (newX + pX), cY = (newY + pY) }
  where
    Cell { cX = pX, cY = pY} = pivot
    (dX, dY) = (x - pX, y - pY)
    (newX, newY) = case direction of
      CW -> (-1 * dY, dX + dY)
      CCW -> (dX + dY, -1 * dX)

-- I am not very proud of this function, but enough is enough
centerUnit :: Int -> Unit -> Unit
centerUnit width unit =
    unit { uMembers = newMembers, uPivot = newPivot }
  where
    newMembers = map centeringMove $ uMembers unit
    newPivot = centeringMove $ uPivot unit
    centeringMove = moveCellBy delta 0
    delta = (minRightDistance - minLeftDistance) `div` 2
    minRightDistance = minimum $ map rightDistance $ uMembers unit
    minLeftDistance = minimum $ map leftDistance $ uMembers unit
    leftDistance c = fst $ unskewedCoords c
    rightDistance c = (width-1) - (fst $ unskewedCoords c)

isValid :: GameState -> Bool
isValid state = all validCell $ (uMembers . gCurrentUnit) state
  where
    validCell c = inBounds c && collisionFree c
    inBounds c = checkBounds c (gWidth state) (gHeight state)
    collisionFree c = notElem c $ gFilled state

checkBounds :: Cell -> Int -> Int -> Bool
checkBounds cell width height =
    (x >= 0) && (x < width) && (y >= 0) && (y < height)
  where
    (x,y) = unskewedCoords cell

-- hexagonal coordinate system is very convinient
-- but it has one downside – x coordinates are skewed
-- so we need to normalize before checking vs bounds
unskewedCoords :: Cell -> (Int, Int)
unskewedCoords Cell {cX = x, cY = y} = (x + (y `quot` 2), y)
