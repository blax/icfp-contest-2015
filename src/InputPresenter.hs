module InputPresenter (showProblem) where

import InputTypes
import Control.Monad (when)

showProblem :: Input -> IO ()
showProblem input = do
  putStrLn "Map"
  showCells (iFilled input)
  putStrLn "Units"
  flip mapM_ (iUnits input) $ \unit -> do
    showCells' ((uPivot unit, '⬡') : map (\x -> (x, '⬢')) (uMembers unit))
    putStrLn ""
  putStrLn "Source length"
  print (iSourceLength input)

showCells :: [Cell] -> IO ()
showCells = showCells' . map (\x -> (x, '⬢'))

showCells' :: [(Cell, Char)] -> IO ()
showCells' [] = return ()
showCells' cells' = do
  flip mapM_ [minY..maxY] $ \y -> do
    when (odd y) $ putStr " "
    flip mapM_ [minX..maxX] $ \x -> do
      case lookup (Cell x y) cells' of
        Just c -> putStr [c]
        Nothing -> putStr " "
      putStr " "
    putStrLn ""

  where minX = minimum (map cX cells)
        maxX = maximum (map cX cells)
        minY = minimum (map cY cells)
        maxY = maximum (map cY cells)
        cells = map fst cells'
