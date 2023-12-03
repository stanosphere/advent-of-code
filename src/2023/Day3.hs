{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Day3 where

import Data.Char (isDigit)
import Data.Map qualified as M (Map, fromList, member)

type Coords = (Int, Int)

type SymbolMap = M.Map Coords Char

-- but some numbers appear twice sooooo we can't store this as a map! (well we could but lists are easier)
type NumberMap = [(Int, [Coords])]

-- 301720 is too low
-- 323508 is too low
-- 535078 is correct
part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input3.txt"
  let symbols = getSymbolCoords input
  print . sum . map fst . filter (isAdjacentToSymbol symbols . snd) . getNumbers $ input

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

-- might have been nicer as a list comp actually
-- don't really need the map for part1 but I suspect the symbols will mean something in part 2
getSymbolCoords :: [String] -> SymbolMap
getSymbolCoords =
  M.fromList
    . filter (isSymbol . snd)
    . concat
    . zipWith (\y xs -> zipWith (\x c -> ((x, y), c)) [0 ..] xs) [0 ..]
  where
    isSymbol c = c /= '.' && (not . isDigit $ c)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)