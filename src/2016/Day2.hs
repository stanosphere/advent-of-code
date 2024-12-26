module Day2 where

import Data.Char (intToDigit)
import Data.Foldable (Foldable (foldl'))

data Move = U | D | L | R

type Coord = (Int, Int)

part1 :: IO String
part1 = map intToDigit . drop 1 . map coordToInt . processAllMoves <$> getInput

processAllMoves :: [[Move]] -> [Coord]
processAllMoves = scanl processMoves (1, 1)

processMoves :: Coord -> [Move] -> Coord
processMoves = foldl' applyMove

applyMove :: Coord -> Move -> Coord
applyMove (x, y) U = if inBounds (x, y - 1) then (x, y - 1) else (x, y)
applyMove (x, y) D = if inBounds (x, y + 1) then (x, y + 1) else (x, y)
applyMove (x, y) L = if inBounds (x - 1, y) then (x - 1, y) else (x, y)
applyMove (x, y) R = if inBounds (x + 1, y) then (x + 1, y) else (x, y)

inBounds :: Coord -> Bool
inBounds (x, y) = x `elem` [0 .. 2] && y `elem` [0 .. 2]

coordToInt :: Coord -> Int
coordToInt (0, 0) = 1
coordToInt (1, 0) = 2
coordToInt (2, 0) = 3
coordToInt (0, 1) = 4
coordToInt (1, 1) = 5
coordToInt (2, 1) = 6
coordToInt (0, 2) = 7
coordToInt (1, 2) = 8
coordToInt (2, 2) = 9
coordToInt c = error ("who ordered that? " ++ show c)

getInput :: IO [[Move]]
getInput = map parseLine . lines <$> readFile "./fixtures/input2.txt"
  where
    parseLine :: String -> [Move]
    parseLine = map parseMove
    parseMove 'U' = U
    parseMove 'D' = D
    parseMove 'L' = L
    parseMove 'R' = R
    parseMove _ = undefined