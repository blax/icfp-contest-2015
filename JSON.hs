{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson   as A
import qualified Network.Wreq as W
import           Lens.Micro   ((^.))
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString.Lazy as BL
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
             deriving Show

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
  parseJSON (A.Object v) = Cell <$> v A..: "x" <*> v A..: "y"
  parseJSON _ = mzero

instance A.FromJSON Unit where
  parseJSON (A.Object v) = Unit <$> v A..: "members"
                                <*> v A..: "pivot"
  parseJSON _ = mzero

getProblem :: String -> IO (Maybe Input)
getProblem s = do
  r <- W.get s
  return (A.decode (r ^. W.responseBody))

main :: IO ()
main = do
  flip mapM_ [1..12] $ \i -> do
    Just r <- getProblem ("http://icfpcontest.org/problems/problem_"
                          ++ show i
                          ++ ".json")
    print i
    putStrLn "Map"
    showCells (iFilled r)
    putStrLn "Units"
    flip mapM_ (iUnits r) $ \unit -> do
      showCells' ((uPivot unit, 'O') : map (\x -> (x, 'X')) (uMembers unit))
      putStrLn ""
    putStrLn "Source length"
    print (iSourceLength r)

showCells :: [Cell] -> IO ()
showCells = showCells' . map (\x -> (x, 'X'))

showCells' :: [(Cell, Char)] -> IO ()
showCells' [] = return ()
showCells' cells' = do
  flip mapM_ [minY..maxY] $ \y -> do
    flip mapM_ [minX..maxX] $ \x -> do
      case lookup (Cell x y) cells' of
        Just c -> putStr [c]
        Nothing -> putStr " "
    putStrLn ""

  where minX = minimum (map cX cells)
        maxX = maximum (map cX cells)
        minY = minimum (map cY cells)
        maxY = maximum (map cY cells)
        cells = map fst cells'
