module Day1 where

import Data.List (sort, transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Utils.Grouping (frequencies)

part1 :: IO Int
part1 = solvePart1 <$> getInput "./fixtures/input1.txt"

part2 :: IO Int
part2 = solvePart2 <$> getInput "./fixtures/input1.txt"

solvePart1 :: ([Int], [Int]) -> Int
solvePart1 (xs, ys) = sum distances
  where
    getDist x y = abs (x - y)
    distances = zipWith getDist (sort xs) (sort ys)

solvePart2 :: ([Int], [Int]) -> Int
solvePart2 (xs, ys) = sum . map getScore $ xs
  where
    lookupMap = frequencies ys
    getScore x = (* x) . fromMaybe 0 . M.lookup x $ lookupMap

getInput :: FilePath -> IO ([Int], [Int])
getInput filePath = toTuple . map (map read) . transpose . map words . lines <$> readFile filePath
  where
    toTuple :: [a] -> (a, a)
    toTuple [x, y] = (x, y)
    toTuple _ = undefined