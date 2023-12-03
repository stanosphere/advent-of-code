{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Day3 where

import Data.Char (isDigit)
import Data.List (nub)
import Data.Map qualified as M (Map, filter, fromList, keys, member)

type Coords = (Int, Int)

type SymbolMap = M.Map Coords Char

-- but some numbers appear twice sooooo we can't store this as a map! (well we could but lists are easier)
-- as I say for part II I should probably have made this a map
type NumberMap = [(Int, [Coords])]

-- 301720 is too low
-- 323508 is too low
-- 535078 is correct
-- 0.06 secs
part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input3.txt"
  let symbols = getSymbolCoords input
  print . sum . map fst . filter (isAdjacentToSymbol symbols . snd) . getNumbers $ input

-- part 2
-- '*'s are gears id they're adjacent to exactly two part numbers
-- we can uniquely identify numbers by their coordinate sets
-- so for each '*' I just to find which numbers its adjacent to, remembering to do appropriate deduplication
-- and keep hold of them if there are tow, and get rid if not

-- 75312571
-- 2.24 secs, could definitely be more efficient if I didn't overuse lists
part2 :: IO ()
part2 = do
  input <- getLines "./fixtures/input3.txt"
  let numbers = getNumbers input
  print . sum . map (getGearValue numbers) . getGearCandidates $ input

getGearValue :: NumberMap -> Coords -> Int
getGearValue nm coords = case nub . filter (any (isAdjacent coords) . snd) $ nm of
  [a, b] -> fst a * fst b
  _ -> 0
  where
    isAdjacent :: Coords -> Coords -> Bool
    isAdjacent (x0, y0) (x1, y1) = (x1, y1) `elem` ([(x, y) | x <- [x0 - 1, x0, x0 + 1], y <- [y0 - 1, y0, y0 + 1]])

getGearCandidates :: [String] -> [Coords]
getGearCandidates = M.keys . M.filter (== '*') . getSymbolCoords

isAdjacentToSymbol :: SymbolMap -> [Coords] -> Bool
isAdjacentToSymbol sm = any (isAdjacentToSymbol' sm)
  where
    -- remember diagonals count!!!
    isAdjacentToSymbol' :: SymbolMap -> Coords -> Bool
    isAdjacentToSymbol' symbols (x0, y0) = any (`M.member` symbols) ([(x, y) | x <- [x0 - 1, x0, x0 + 1], y <- [y0 - 1, y0, y0 + 1]])

getNumbers :: [String] -> NumberMap
getNumbers = concat . zipWith parseLine [0 ..]
  where
    -- could probably do in terms of foldr
    parseLine :: Int -> String -> NumberMap
    parseLine y str = map (stateToCoords y) . foldl f [] $ zip str [0 ..]
    f :: [State] -> (Char, Int) -> [State]
    f [] (x, i) = [State [x] [i] False | isDigit x]
    f (s : t) (x, i)
      | isDigit x && isComplete s = State [x] [i] False : s : t
      | isDigit x && (not . isComplete $ s) = State (digits s ++ [x]) (xPositions s ++ [i]) False : t
      | otherwise = State (digits s) (xPositions s) True : t
    stateToCoords :: Int -> State -> (Int, [Coords])
    stateToCoords y (State digits xPositions _) = (read digits, map (\x -> (x, y)) xPositions)

data State = State {digits :: [Char], xPositions :: [Int], isComplete :: Bool} deriving (Show)

-- don't really need the map for part1 but I suspect the symbols will mean something in part 2
getSymbolCoords :: [String] -> SymbolMap
getSymbolCoords inp = M.fromList [((x, y), c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs, isSymbol c]
  where
    isSymbol c = c /= '.' && (not . isDigit $ c)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)