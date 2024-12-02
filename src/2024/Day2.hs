module Day2 where

import Data.List (tails)

part1 :: IO Int
part1 = solvePart1 <$> getInput "./fixtures/input2.txt"

solvePart1 :: [[Int]] -> Int
solvePart1 = length . filter isSafe

-- The levels are either all increasing or all decreasing.
-- Any two adjacent levels differ by at least one and at most three.
isSafe :: [Int] -> Bool
isSafe xs = all (`elem` [1 .. 3]) differences || all (`elem` [-3 .. -1]) differences
  where
    differences = map (uncurry (-)) . pairs $ xs

pairs :: [a] -> [(a, a)]
pairs = map (\[x, y] -> (x, y)) . windows 2

-- wrote this for one of the older puzzles and have been using it ever since
windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((n ==) . length) . map (take n) . tails

getInput :: FilePath -> IO [[Int]]
getInput filePath = map (map read . words) . lines <$> readFile filePath