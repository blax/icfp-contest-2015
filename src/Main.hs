module Main where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BL
import Options.Applicative ((<>), Parser, ParserInfo)
import qualified Options.Applicative as O
import Data.List (nub)
import InputTypes
import InputPresenter as IP
import Solver as SO
import Simulation as S

data Options = Options {
    optInputFiles :: [FilePath],
    optTimeLimit :: Int,
    optMemoryLimit :: Int,
    optCoreCount :: Int,
    optPowerPhrases :: [String]}
  deriving (Show)

parseOptions :: Parser Options
parseOptions =
    Options <$>
      O.many (
        O.strOption (
          O.short 'f' <>
          O.metavar "FILENAME" <>
          O.help "Files containing JSON encoded input")) <*>
      O.option O.auto (
        O.short 't' <>
        O.metavar "NUMBER" <>
        O.value 0 <>
        O.help "Time limit, in seconds, to produce output") <*>
      O.option O.auto (
        O.short 'm' <>
        O.metavar "NUMBER" <>
        O.value 0 <>
        O.help "Memory limit, in megabytes, to produce output") <*>
      O.option O.auto (
        O.short 'c' <>
        O.metavar "NUMBER" <>
        O.value 0 <>
        O.help "Number of processor cores available") <*>
      O.many (
        O.strOption (
          O.short 'p' <>
          O.metavar "STRING" <>
          O.help "Phrases of power, as quoted string"))

withInfo :: Parser a -> String -> ParserInfo a
withInfo parser info =
    O.info (O.helper <*> parser) (O.progDesc info)

printProblemFromFile :: FilePath -> IO ()
printProblemFromFile f = do
    Just input <- decode <$> BL.readFile f :: IO (Maybe Input)
    IP.showProblem input
    let state1 = S.initialState input (head . iSourceSeeds $ input)
        lockedPoints = SO.lowLevelSolve state1
    mapM_ print lockedPoints
    print . length . nub $ lockedPoints


main :: IO ()
main = do
    opts <- O.execParser (parseOptions `withInfo` "Solve the problem")
    mapM_ printProblemFromFile $ optInputFiles opts
