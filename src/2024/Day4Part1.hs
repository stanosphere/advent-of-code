module Day4Part1 where

import qualified Data.Map as M
import Data.Maybe (catMaybes)

type Coord = (Int, Int)

type Grid = M.Map Coord Char

-- plan
-- for each X in the word search look in all 8 directions and see if you find the word XMAS
-- build up grid (probably sensible to only take those letters which are in XMAS)
-- find all the X's
-- apply some function in 8 directions

part1 :: IO Int
part1 = countAllXmasOccurrences . toGrid <$> getInput

countAllXmasOccurrences :: Grid -> Int
countAllXmasOccurrences grid =
  sum
    . map (\xPos -> countXmasOccurrences grid xPos stepFunctions)
    . findXPositions
    $ grid

countXmasOccurrences :: Grid -> Coord -> [Coord -> Coord] -> Int
countXmasOccurrences grid xLocation = length . filter (isXmas grid xLocation)

isXmas :: Grid -> Coord -> (Coord -> Coord) -> Bool
isXmas grid xLocation stepFn = word == "XMAS"
  where
    word = catMaybes . take 4 . map (`M.lookup` grid) . iterate stepFn $ xLocation

findXPositions :: Grid -> [Coord]
findXPositions = M.keys . M.filter (== 'X')

-- I could probably have done this as a list comp couldn't I...
stepFunctions :: [Coord -> Coord]
stepFunctions =
  [ \(x, y) -> (x - 1, y - 1),
    \(x, y) -> (x - 1, y),
    \(x, y) -> (x - 1, y + 1),
    \(x, y) -> (x, y - 1),
    \(x, y) -> (x, y + 1),
    \(x, y) -> (x + 1, y - 1),
    \(x, y) -> (x + 1, y),
    \(x, y) -> (x + 1, y + 1)
  ]

toGrid :: [String] -> Grid
toGrid grid =
  M.fromList
    [ ((i, j), char)
      | (j, row) <- zipWithIndex grid,
        (i, char) <- zipWithIndex row,
        char `elem` "XMAS"
    ]
  where
    -- based on the function of the same name in scala
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input4.txt"