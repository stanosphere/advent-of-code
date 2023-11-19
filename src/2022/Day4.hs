module Day4 where

import Data.List.Split (splitOn)

data Range = Range
  { start :: Int,
    end :: Int
  }
  deriving (Show)

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input4.txt"
  print . length . filter doesFullyContain . map parseLine $ contents

part2 :: IO ()
part2 = do
  contents <- getLines "./fixtures/input4.txt"
  print . length . filter doesOverlap . map parseLine $ contents

doesFullyContain :: (Range, Range) -> Bool
doesFullyContain (Range start1 end1, Range start2 end2) =
  start1 <= start2 && end1 >= end2 || start2 <= start1 && end2 >= end1

doesOverlap :: (Range, Range) -> Bool
doesOverlap (Range start1 end1, Range start2 end2) =
  start1 <= end2 && end1 >= start2

parseLine :: String -> (Range, Range)
parseLine s =
  let [left, right] = splitOn "," s in (parseRange left, parseRange right)

parseRange :: String -> Range
parseRange s =
  let [lower, upper] = splitOn "-" s in Range (read lower) (read upper)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
