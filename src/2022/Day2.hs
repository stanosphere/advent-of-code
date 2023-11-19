module Day2 where

import Data.List.Split (splitOn)
import System.IO ()

data Move = Rock | Paper | Scissors deriving (Show)

data DerisedOutcome = Win | Lose | Draw deriving (Show)

data Turn = Turn
  { opponentMove :: Move,
    myMove :: Move
  }
  deriving (Show)

data OpponentMoveWithOutcome = OMWO
  { enemyMove :: Move,
    desiredOutcome :: DerisedOutcome
  }

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input2.txt"
  print . fmap (sum . map getMyScore) . traverse turnFromMoveData $ contents

part2 :: IO ()
part2 = do
  contents <- getLines "./fixtures/input2.txt"
  print
    . fmap (sum . map getMyScore)
    . traverse turnFromMoveAndOutcomeData
    $ contents

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

turnFromMoveData :: String -> Maybe Turn
turnFromMoveData s = case splitOn " " s of
  [x, y] -> do
    a <- opponentMoveFromString x
    b <- myMoveFromString y
    return $ Turn a b
  _ -> Nothing

turnFromMoveAndOutcomeData :: String -> Maybe Turn
turnFromMoveAndOutcomeData s = case splitOn " " s of
  [x, y] -> do
    move <- opponentMoveFromString x
    outcome <- desiredOutcomeFromString y
    return $ opponentMoveWithOutcomeToTurn $ OMWO move outcome
  _ -> Nothing

opponentMoveWithOutcomeToTurn :: OpponentMoveWithOutcome -> Turn
opponentMoveWithOutcomeToTurn (OMWO x Draw) = Turn x x
opponentMoveWithOutcomeToTurn (OMWO Rock Win) = Turn Rock Paper
opponentMoveWithOutcomeToTurn (OMWO Rock Lose) = Turn Rock Scissors
opponentMoveWithOutcomeToTurn (OMWO Paper Win) = Turn Paper Scissors
opponentMoveWithOutcomeToTurn (OMWO Paper Lose) = Turn Paper Rock
opponentMoveWithOutcomeToTurn (OMWO Scissors Win) = Turn Scissors Rock
opponentMoveWithOutcomeToTurn (OMWO Scissors Lose) = Turn Scissors Paper

myMoveFromString :: String -> Maybe Move
myMoveFromString "X" = Just Rock
myMoveFromString "Y" = Just Paper
myMoveFromString "Z" = Just Scissors
myMoveFromString _ = Nothing

opponentMoveFromString :: String -> Maybe Move
opponentMoveFromString "A" = Just Rock
opponentMoveFromString "B" = Just Paper
opponentMoveFromString "C" = Just Scissors
opponentMoveFromString _ = Nothing

desiredOutcomeFromString :: String -> Maybe DerisedOutcome
desiredOutcomeFromString "X" = Just Lose
desiredOutcomeFromString "Y" = Just Draw
desiredOutcomeFromString "Z" = Just Win
desiredOutcomeFromString _ = Nothing

getMyScore :: Turn -> Int
getMyScore (Turn Rock Rock) = 1 + 3
getMyScore (Turn Rock Paper) = 2 + 6
getMyScore (Turn Rock Scissors) = 3 + 0
getMyScore (Turn Paper Rock) = 1 + 0
getMyScore (Turn Paper Paper) = 2 + 3
getMyScore (Turn Paper Scissors) = 3 + 6
getMyScore (Turn Scissors Rock) = 1 + 6
getMyScore (Turn Scissors Paper) = 2 + 0
getMyScore (Turn Scissors Scissors) = 3 + 3
