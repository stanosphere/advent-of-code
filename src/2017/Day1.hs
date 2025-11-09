module Day1 where

import Data.Char (digitToInt)
import Utils.Grouping (pairs)

part1 :: IO Int
part1 = do
  input <- getInput "./fixtures/input1.txt"
  return
    . sum
    . map (\(x, y) -> if x == y then x else 0)
    . pairs
    . take (1 + length input)
    . cycle
    $ input

part2 = do
  input <- getInput "./fixtures/input1.txt"
  return
    . sum
    . map (\(x, y) -> if x == y then x else 0)
    . zipWithOffset ((`div` 2) . length $ input)
    $ input

-- offsets a list by n, and zips with the original
-- resulting list has the same length as the original
zipWithOffset :: Int -> [a] -> [(a, a)]
zipWithOffset n xs = zip xs . drop n . cycle $ xs

getInput :: FilePath -> IO [Int]
getInput filePath = map digitToInt . head . lines <$> readFile filePath
