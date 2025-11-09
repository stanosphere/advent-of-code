module Day1 where

import Data.Char (digitToInt)

part1 :: IO Int
part1 = do
  input <- getInput "./fixtures/input1.txt"
  let n = 1
  return . solve n $ input

part2 :: IO Int
part2 = do
  input <- getInput "./fixtures/input1.txt"
  let n = (`div` 2) . length $ input
  return . solve n $ input

solve :: Int -> [Int] -> Int
solve n = sum . map (uncurry keepIfMatch) . zipWithOffset n
  where
    keepIfMatch x y = if x == y then x else 0

-- offsets a list by n, and zips with the original
-- resulting list has the same length as the original
zipWithOffset :: Int -> [a] -> [(a, a)]
zipWithOffset n xs = zip xs . drop n . cycle $ xs

getInput :: FilePath -> IO [Int]
getInput filePath = map digitToInt . head . lines <$> readFile filePath
