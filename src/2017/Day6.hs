module Day6 where

import Data.List (find)
import Data.List.Extra (maximumOn)
import qualified Data.Set as S
  ( empty,
    insert,
    size,
  )

type Banks = [Int]

part1 :: IO (Maybe Int)
part1 =
  fmap ((\x -> x - 1) . snd)
    . find (\(s, n) -> S.size s /= n)
    . scanl (\(s, n) x -> (S.insert x s, n + 1)) (S.empty, 0)
    . iterate redistribute
    <$> getInput

redistribute :: Banks -> Banks
redistribute banks = zipWith (+) deltas banks
  where
    (index, memory) = getBankToRedistribute banks
    len = length banks
    deltas = take len . drop (len - index - 1) . cycle $ getMemoryToDistribute memory len

getBankToRedistribute :: Banks -> (Int, Int)
getBankToRedistribute = maximumOn (\(index, value) -> (value, -index)) . zip [0 ..]

getMemoryToDistribute :: Int -> Int -> [Int]
getMemoryToDistribute memory len =
  zipWith3
    (\x y z -> x + y + z)
    memoryToRemove
    baseMemory
    leftOvers
  where
    baseMemory = replicate len (memory `div` len)
    leftOvers = replicate (memory `rem` len) 1 ++ repeat 0
    memoryToRemove = replicate (len - 1) 0 ++ [memory * (-1)]

getInput :: IO Banks
getInput =
  map read
    . words
    <$> readFile "./fixtures/input6.txt"