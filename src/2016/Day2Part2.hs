module Day2Part2 where

import Data.Foldable (Foldable (foldl'))

data Move = U | D | L | R

type Coord = (Int, Int)

part2 :: IO String
part2 = drop 1 . map coordToChar . processAllMoves <$> getInput

processAllMoves :: [[Move]] -> [Coord]
processAllMoves = scanl processMoves (0, 0)

processMoves :: Coord -> [Move] -> Coord
processMoves = foldl' applyMove

applyMove :: Coord -> Move -> Coord
applyMove (x, y) U = if inBounds (x, y - 1) then (x, y - 1) else (x, y)
applyMove (x, y) D = if inBounds (x, y + 1) then (x, y + 1) else (x, y)
applyMove (x, y) L = if inBounds (x - 1, y) then (x - 1, y) else (x, y)
applyMove (x, y) R = if inBounds (x + 1, y) then (x + 1, y) else (x, y)

inBounds :: Coord -> Bool
inBounds (x, y) = abs x + abs y <= 2

{-

    1
  2 3 4
5 6 7 8 9
  A B C
    D

 -}

coordToChar :: Coord -> Char
coordToChar (-2, 0) = '5'
coordToChar (-1, 0) = '6'
coordToChar (0, 0) = '7'
coordToChar (1, 0) = '8'
coordToChar (2, 0) = '9'
coordToChar (-1, -1) = '2'
coordToChar (0, -1) = '3'
coordToChar (1, -1) = '4'
coordToChar (-1, 1) = 'A'
coordToChar (0, 1) = 'B'
coordToChar (1, 1) = 'C'
coordToChar (0, 2) = 'D'
coordToChar (0, -2) = '1'
coordToChar c = error ("who ordered that? " ++ show c)

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