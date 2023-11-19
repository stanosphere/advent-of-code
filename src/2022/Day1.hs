module Day1 where

import Data.List (sort)
import Data.List.Split (splitOn)
import System.IO ()

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input1.txt"
  print . maximum . map (sum . stringsToInts) . splitOn [""] $ contents

part2 :: IO ()
part2 = do
  contents <- getLines "./fixtures/input1.txt"
  print
    . sum
    . take 3
    . reverse
    . sort
    . map (sum . stringsToInts)
    . splitOn [""]
    $ contents

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

stringsToInts :: [String] -> [Integer]
stringsToInts = map read
