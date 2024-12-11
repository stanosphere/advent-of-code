module Day11 where

import Data.List.Split (splitOn)
import qualified Data.Map as M

-- apparently the official name is Multiset https://en.wikipedia.org/wiki/Multiset
type Bag = M.Map Int Int

part1 :: IO Int
part1 = length . (!! 25) . iterate step <$> getInput

part2 :: IO Int
part2 = totalStones . (!! 75) . iterate step' . frequencies <$> getInput

totalStones :: Bag -> Int
totalStones = M.foldr (+) 0

step :: [Int] -> [Int]
step = concatMap transformStone

-- not sure if this is nested enough lol
step' :: Bag -> Bag
step' = M.foldrWithKey folder M.empty
  where
    folder :: Int -> Int -> Bag -> Bag
    folder stoneValue stoneCount bag = foldr insert bag newValues
      where
        newValues = transformStone stoneValue
        insert newValue = M.insertWith (+) newValue stoneCount

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

-- taken from Day 1 and Day 10 of this year, might be worth popping it in some util module now that it's used 3 times
frequencies :: (Ord a) => [a] -> M.Map a Int
frequencies = foldr incrementMap M.empty
  where
    incrementMap x = M.insertWith (+) x 1
