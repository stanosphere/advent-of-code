module Day1 where

import Control.Monad ((<=<))
import Data.Monoid (Sum (Sum))

part1 :: IO ()
part1 = (print <=< fmap (foldMap (Sum . getMass))) input

part2 :: IO ()
part2 = (print <=< fmap (foldMap (Sum . getMassRec))) input

getMass :: Int -> Int
getMass i = (i `div` 3) - 2

getMassRec :: Int -> Int
getMassRec x = go 0 x
  where
    go :: Int -> Int -> Int
    go acc x = let m = getMass x in if m < 0 then acc else go (acc + m) m

input :: IO [Int]
input = (fmap (map read . lines) . readFile) "./fixtures/input1.txt"
