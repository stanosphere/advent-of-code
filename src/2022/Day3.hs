module Day3 where

import Data.Char (ord)
import Data.List.Split (chunksOf)
import qualified Data.Set as S

part1 :: IO ()
part1 = do
  contents <- getLines "./fixtures/input3.txt"
  print
    . sum
    . map
      (sum . map letterToPriority . S.toList . findIntersection . splitInHalf)
    $ contents

part2 :: IO ()
part2 = do
  contents <- getLines "./fixtures/input3.txt"
  print
    . sum
    . map (letterToPriority . head . S.toList . intersectAll)
    . chunksOf 3
    . map S.fromList
    $ contents

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

splitInHalf :: [a] -> ([a], [a])
splitInHalf xs = splitAt (length xs `div` 2) xs

findIntersection :: (Ord a) => ([a], [a]) -> S.Set a
findIntersection (xs, ys) = S.intersection (S.fromList xs) (S.fromList ys)

letterToPriority :: Char -> Int
letterToPriority c = if c `elem` ['a' .. 'z'] then ord c - 96 else ord c - 38

intersectAll :: (Ord a) => [S.Set a] -> S.Set a
intersectAll [] = S.empty
intersectAll (x : xs) = foldr S.intersection x xs
