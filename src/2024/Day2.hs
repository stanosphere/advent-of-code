module Day2 where

import Data.List (inits, tails)

part1 :: IO Int
part1 = length . filter isSafe <$> getInput

part2 :: IO Int
part2 = length . filter isSafe' <$> getInput

-- same as isSafe but can tolerate a single bad level
-- something _might_ be safe if
-- - there is exactly one bad difference
-- - there is exactly two bad differences and they are next to each other
-- however just to solve the problem I think the easiest thing will be to just remove each element from a bad list and see what happens...
-- still runs in like 0.04 secs so it's calm
isSafe' :: [Int] -> Bool
isSafe' xs = isSafe xs || (any isSafe . withOneElementRemoved $ xs)

withOneElementRemoved :: [a] -> [[a]]
withOneElementRemoved xs = zipWith (++) (inits xs) (drop 1 . tails $ xs)

-- The levels are either all increasing or all decreasing.
-- Any two adjacent levels differ by at least one and at most three.
isSafe :: [Int] -> Bool
isSafe xs = all (`elem` [1 .. 3]) differences || all (`elem` [-3 .. -1]) differences
  where
    differences = map (uncurry (-)) . pairs $ xs

pairs :: [a] -> [(a, a)]
pairs = map (\[x, y] -> (x, y)) . windows 2

-- wrote this for one of the older puzzles and have been using it ever since
windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((n ==) . length) . map (take n) . tails

getInput :: IO [[Int]]
getInput = map (map read . words) . lines <$> readFile "./fixtures/input2.txt"