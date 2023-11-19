module Day1Part2 where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.List (tails)
import System.IO ()

main :: IO ()
main = do
  contents <- getLines "../fixtures/input1.txt"
  let q = map sum ((windows 3 . linesToInts) contents)
  (print . count hasIncreased . windows 2) q

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

linesToInts :: [String] -> [Integer]
linesToInts = map read

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

hasIncreased :: [Integer] -> Bool
hasIncreased xs = head xs < last xs