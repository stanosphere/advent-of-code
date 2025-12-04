module Day1 where

import Data.List (foldl', scanl')
import Text.Parsec as P (char, digit, many1, newline, sepBy, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.BenchMark (runBenchMark)

data Direction = L | R deriving (Show)

data Command = Command {_direction :: Direction, _amount :: Int} deriving (Show)

data SafeState = SafeState {_position :: Int, _timesCrossedZero :: Int} deriving (Show)

-- part 2 we care about how many times we cross zero, not just exact hits
-- I think we can break this down like so
-- - count number of full rotations (i.e. divide by 100)
-- -- for each full rotation we know that we must cross zero
-- - for the remainder work out if we cross zero or not
-- -- for left rotations do some subtraction and see if we end up with a -ve number
-- -- for right rotations do some addition and see if we cross 100
-- -- there is no doubt a way to do this with modular arithmetic too but I can't think of what it would be

part1 :: IO ()
part1 = do
  input <- getInput
  runBenchMark findTimesWeHitZero input

part2 :: IO ()
part2 = do
  input <- getInput
  runBenchMark findTimesWeHitZero input

findTimesWeCrossZero :: [Command] -> Int
findTimesWeCrossZero = _timesCrossedZero . foldl' applyCommand' (SafeState 50 0)

applyCommand' :: SafeState -> Command -> SafeState
applyCommand' (SafeState position timesCrossedZero) command =
  SafeState
    (applyCommand position command)
    (timesCrossedZero + workOutTimesWeCrossZero position command)

workOutTimesWeCrossZero :: Int -> Command -> Int
workOutTimesWeCrossZero position (Command direction amount) = fullRotations + if remainderCrossesZero then 1 else 0
  where
    fullRotations = amount `div` 100
    amountMod100 = amount `mod` 100
    remainderCrossesZero = case direction of
      L -> position - amountMod100 <= 0 && position /= 0
      R -> position + amountMod100 >= 100

findTimesWeHitZero :: [Command] -> Int
findTimesWeHitZero = length . filter (== 0) . scanl' applyCommand 50

applyCommand :: Int -> Command -> Int
applyCommand position (Command L x) = (position - x) `mod` 100
applyCommand position (Command R x) = (position + x) `mod` 100

getInput :: IO [Command]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input1.txt"
  where
    inputParser :: Parser [Command]
    inputParser = commandParser `P.sepBy` P.newline

    commandParser :: Parser Command
    commandParser = Command <$> directionParser <*> intParser
      where
        directionParser = (L <$ P.char 'L') P.<|> (R <$ P.char 'R')
        intParser = read <$> P.many1 P.digit

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res