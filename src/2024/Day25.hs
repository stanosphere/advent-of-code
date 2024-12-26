module Day25 where

import Data.Bifunctor (bimap)
import Data.List (partition, transpose)
import Data.List.Split (splitOn)

part1 :: IO Int
part1 = do
  (locks, keys) <-
    bimap (map parseLock) (map parseKey)
      . partition ((== ".....") . head)
      . splitOn [""]
      . lines
      <$> readFile "./fixtures/input25.txt"

  return (length [() | lock <- locks, key <- keys, doesFit lock key])

parseLock :: [[Char]] -> [Int]
parseLock = map ((6 -) . length . takeWhile (== '.')) . transpose

parseKey :: [[Char]] -> [Int]
parseKey = map ((6 -) . length . dropWhile (== '#')) . transpose

doesFit :: [Int] -> [Int] -> Bool
doesFit locks = all (\(x, y) -> x + y < 6) . zip locks
