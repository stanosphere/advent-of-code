module Day9 where

import Data.List
import Data.List.Split (splitOn)

part1 = do
  input <- getLines "./fixtures/input9.txt"
  let xs = sum . map (getLastNum . parseLine) $ input
  return xs

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