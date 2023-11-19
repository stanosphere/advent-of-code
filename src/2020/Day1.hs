module Day1 where

import Data.List (find)

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input1.txt"
  let ints = stringsToInts contents
  let found = findValuesThatSum 2020 ints ints
  let res = fmap (uncurry (*)) found
  print res

part2 :: IO ()
part2 = do
  contents <- getLines "./fixtures/input1.txt"
  let ints = stringsToInts contents
  let found = findValuesThatSum' 2020 ints ints ints
  let res = fmap (\(x, y, z) -> x * y * z) found
  print res

findValuesThatSum :: Int -> [Int] -> [Int] -> Maybe (Int, Int)
findValuesThatSum n xs ys =
  find (\(x, y) -> x + y == n) [(x, y) | x <- xs, y <- ys]

findValuesThatSum' :: Int -> [Int] -> [Int] -> [Int] -> Maybe (Int, Int, Int)
findValuesThatSum' n xs ys zs =
  find (\(x, y, z) -> x + y + z == n) [(x, y, z) | x <- xs, y <- ys, z <- zs]

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

stringsToInts :: [String] -> [Int]
stringsToInts = map read
