module Day13 where

import Data.List (transpose)
import Data.List.Split (splitOn)

data Reflections = Reflections {vertical :: [Int], horizontal :: [Int]} deriving (Show)

-- 42974
-- 0.02 secs
part1 :: IO Int
part1 = do
  patterns <- getPatterns "./fixtures/input13.txt"
  return
    . foldl (\count (Reflections v h) -> count + (sum . map (* 1) $ v) + (sum . map (* 100) $ h)) 0
    . map getBothReflections
    $ patterns

getBothReflections :: Eq a => [[a]] -> Reflections
getBothReflections xs = Reflections vertical horizontal
  where
    vertical = getAllReflections . transpose $ xs
    horizontal = getAllReflections xs

getAllReflections :: Eq a => [a] -> [Int]
getAllReflections = map fst . filter (uncurry areReflections . snd) . getAllSplits
  where
    areReflections :: Eq a => [a] -> [a] -> Bool
    areReflections left right = all (uncurry (==)) (zip (reverse left) right)
    getAllSplits :: [a] -> [(Int, ([a], [a]))]
    getAllSplits xs = map (\i -> (i, splitAt i xs)) [1 .. length xs - 1]

getPatterns :: FilePath -> IO [[String]]
getPatterns filePath = splitOn [[]] . lines <$> readFile filePath