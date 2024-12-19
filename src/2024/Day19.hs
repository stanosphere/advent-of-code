module Day19 where

import Data.List.Split (splitOn)

type TowelPattern = String

type TowelDesign = String

type PatternMap = [(Int, TowelPattern)]

part1 :: IO Int
part1 = do
  (patterns, designs) <- getInput
  return . length . filter (isPossible patterns) $ designs

isPossible :: PatternMap -> TowelDesign -> Bool
isPossible patterns design = any (isPossible' patterns design) patterns

isPossible' :: PatternMap -> TowelDesign -> (Int, TowelPattern) -> Bool
isPossible' _ [] (_, _) = True
isPossible' patterns design (len, pattern) = take len design == pattern && isPossible patterns (drop len design)

getInput :: IO (PatternMap, [TowelDesign])
getInput = parseInput . lines <$> readFile "./fixtures/input19.txt"

parseInput :: [String] -> (PatternMap, [TowelDesign])
parseInput xs = (patterns, designs)
  where
    patterns = map (\x -> (length x, x)) . splitOn ", " . head $ xs
    designs = drop 2 xs