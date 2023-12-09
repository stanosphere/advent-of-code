module Day9 where

import Data.List (tails)
import Data.List.Split (splitOn)

part1 :: IO Int
part1 = sum . map (getLastNum . parseLine) <$> inputLines

part2 :: IO Int
part2 = sum . map (getLastNum . reverse . parseLine) <$> inputLines

inputLines :: IO [String]
inputLines = getLines "./fixtures/input9.txt"

parseLine :: String -> [Int]
parseLine = map read . splitOn " "

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

getLastNum :: [Int] -> Int
getLastNum = sum . map last . allDiffs

allDiffs :: [Int] -> [[Int]]
allDiffs = takeWhile (not . all (== 0)) . iterate diffs

diffs :: [Int] -> [Int]
diffs = map (uncurry (-)) . pairs

pairs :: [a] -> [(a, a)]
pairs = map (\[x1, x2] -> (x2, x1)) . windows 2

windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((n ==) . length) . map (take n) . tails