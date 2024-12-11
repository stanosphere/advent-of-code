module Day11 where

import Data.List.Split (splitOn)

part1 :: IO Int
part1 = length . (!! 25) . iterate step <$> getInput

part2 :: IO Int
part2 = length . (!! 75) . iterate step <$> getInput

step :: [Int] -> [Int]
step = concatMap transformStone

transformStone :: Int -> [Int]
transformStone x
  | x == 0 = [1]
  | even . length . show $ x = map read . splitInHalf . show $ x
  | otherwise = [x * 2024]

splitInHalf :: [a] -> [[a]]
splitInHalf xs = case splitAt splitIndex xs of (l, r) -> [l, r]
  where
    splitIndex = (length xs + 1) `div` 2

getInput :: IO [Int]
getInput = map read . splitOn " " <$> readFile "./fixtures/input11.txt"
