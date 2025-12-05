module Day5 where

import Data.List (sortOn)

data Interval = Interval {_start :: Int, _end :: Int} deriving (Show)

part1 :: IO Int
part1 = do
  (intervals, ids) <- getInput
  let mergedIntervals = mergeIntervals intervals
  return . length . filter (\id -> any (\(Interval lo hi) -> id >= lo && id <= hi) mergedIntervals) $ ids

part2 :: IO Int
part2 = do
  (intervals, _) <- getInput
  return . sum . map (\(Interval lo hi) -> hi - lo + 1) . mergeIntervals $ intervals

-- this is a standard algorithm for merging intervals
-- from the example we have [3-5, 10-14, 16-20, 12-18] -> [3-5, 10-20]
-- do we need this, maybe not lol?
mergeIntervals :: [Interval] -> [Interval]
mergeIntervals = mergeIntervals' . sortOn _start
  where
    mergeIntervals' :: [Interval] -> [Interval]
    mergeIntervals' (Interval lo1 hi1 : Interval lo2 hi2 : rest)
      | hi1 >= lo2 - 1 =
          mergeIntervals' (Interval (min lo1 lo2) (max hi1 hi2) : rest)
    mergeIntervals' (interval : rest) = interval : mergeIntervals' rest
    mergeIntervals' [] = []

getInput :: IO ([Interval], [Int])
getInput = do
  rawInput <- lines <$> readFile "./fixtures/input5.txt"
  let intervals = map parseInterval . takeWhile (elem '-') $ rawInput
  let ids = map read . drop 1 . dropWhile (elem '-') $ rawInput
  return (intervals, ids)
  where
    parseInterval :: String -> Interval
    parseInterval x =
      Interval
        (read . takeWhile (/= '-') $ x)
        (read . drop 1 . dropWhile (/= '-') $ x)