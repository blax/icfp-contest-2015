{-# LANGUAGE OverloadedStrings #-}

module InputTypes where

import qualified Data.Aeson   as A
import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (mzero, when)

data Input = Input { iId           :: Int
                   , iUnits        :: [Unit]
                   , iWidth        :: Int
                   , iHeight       :: Int
                   , iFilled       :: [Cell]
                   , iSourceLength :: Int
                   , iSourceSeeds  :: [Int] }
             deriving Show

data Cell = Cell { cX :: Int
                 , cY :: Int }
             deriving (Show, Eq)

data Unit = Unit { uMembers :: [Cell]
                 , uPivot   :: Cell }
             deriving (Show, Eq)

instance A.FromJSON Input where
  parseJSON (A.Object v) = Input <$> v A..: "id"
                                <*> v A..: "units"
                                <*> v A..: "width"
                                <*> v A..: "height"
                                <*> v A..: "filled"
                                <*> v A..: "sourceLength"
                                <*> v A..: "sourceSeeds"
  parseJSON _ = mzero

instance A.FromJSON Cell where
  parseJSON (A.Object v) = transformCoords <$> (Cell <$> v A..: "x" <*> v A..: "y")

  parseJSON _ = mzero

instance A.FromJSON Unit where
  parseJSON (A.Object v) = Unit <$> v A..: "members"
                                <*> v A..: "pivot"
  parseJSON _ = mzero

transformCoords :: Cell -> Cell
transformCoords c = c { cX = x', cY = y'} where
  x' = x - (y `div` 2)
  y' = y
  (x, y) = (cX c, cY c)
