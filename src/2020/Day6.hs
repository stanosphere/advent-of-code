module Day6 where

import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, intersection, size, unions)

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input6.txt"
  print . sum . map (length . nub . concat) . splitOn [""] $ contents

part1' :: IO ()
part1' = do
  contents <- getLines "./fixtures/input6.txt"
  print . sum . map (size . unions . map fromList) . splitOn [""] $ contents

part2' :: IO ()
part2' = do
  contents <- getLines "./fixtures/input6.txt"
  print . sum . map (size . intersections . map fromList) . splitOn [""] $ contents

intersections :: (Foldable f, Ord a) => f (Set a) -> Set a
intersections = foldl1 intersection

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
