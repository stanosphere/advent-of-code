module Day9 where

import Data.List (tails)
import Data.List.Split (splitOn)

part1 :: IO Int
part1 = sum . map (getLastNum . parseLine) . lines <$> readFile "./fixtures/input9.txt"

part2 :: IO Int
part2 = sum . map (getLastNum . reverse . parseLine) . lines <$> readFile "./fixtures/input9.txt"

parseLine :: String -> [Int]
parseLine = map read . splitOn " "

getLastNum :: [Int] -> Int
getLastNum = sum . map last . allDiffs
  where
    allDiffs :: [Int] -> [[Int]]
    allDiffs = takeWhile (not . all (== 0)) . iterate diffs

    diffs :: [Int] -> [Int]
    diffs = map (uncurry . flip $ (-)) . pairs

    pairs :: [a] -> [(a, a)]
    pairs = map (\[x1, x2] -> (x1, x2)) . windows 2

    -- essentially scala's sliding, kinda surprised I couldn't find a library function
    windows :: Int -> [a] -> [[a]]
    windows n = takeWhile ((n ==) . length) . map (take n) . tails