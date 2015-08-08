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
    getUnit n = (iUnits i) !! (mod n $ length $ iUnits i)
    unitIndexes = take (iSourceLength i) $ randoms $ mkLCGen seed

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
moveCell move Cell { cX = x, cY = y} =
    Cell { cX = newX, cY = newY }
  where
    (newX, newY) = case move of
      E  -> (x + 1, y)
      W  -> (x - 1, y)
      SE -> (x, y + 1)
      SW -> (x - 1, y + 1)

rotateCell :: Cell -> RotationCommand -> Cell -> Cell
rotateCell pivot direction Cell { cX = x, cY = y } =
    Cell { cX = (newX + pX), cY = (newY + pY) }
  where
    Cell { cX = pX, cY = pY} = pivot
    (dX, dY) = (x - pX, y - pY)
    (newX, newY) = case direction of
      CW -> (-1 * dY, dX + dY)
      CCW -> (dX + dY, -1 * dX)
