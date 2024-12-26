module Day10 where

import Data.Char (digitToInt)
import Data.List (group)

part1 :: IO Int
part1 = length . (!! 40) . iterate step <$> getInput

part2 :: IO Int
part2 = length . (!! 50) . iterate step <$> getInput

getInput :: IO [Int]
getInput = map digitToInt <$> readFile "./fixtures/input10.txt"

step :: [Int] -> [Int]
step = concatMap (\g -> [length g, head g]) . group