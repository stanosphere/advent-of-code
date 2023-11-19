module Day6 where

import Data.List
  ( find,
    nub,
    tails,
  )

part1 :: IO ()
part1 = getInput "./fixtures/input6.txt" >>= (print . getIndex 4)

part2 :: IO ()
part2 = getInput "./fixtures/input6.txt" >>= (print . getIndex 14)

getIndex :: Int -> String -> Maybe Int
getIndex n = fmap fst . find (areAllDifferent . snd) . zip [n ..] . windows n

getInput :: FilePath -> IO String
getInput filePath = fmap (head . lines) (readFile filePath)

-- I wanted this to work on unbounded lists and I think it does
windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((n ==) . length) . map (take n) . tails

areAllDifferent :: Ord a => [a] -> Bool
areAllDifferent xs = nub xs == xs
