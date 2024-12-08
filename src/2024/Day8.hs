module Day8 where

import Data.List (nub)
import qualified Data.Map as M
import Utils.Grouping (groupMap)

type Coord = (Int, Int)

type Grid = [(Coord, Char)]

data GridSize = GS {_width :: Int, _height :: Int} deriving (Show)

data GridInfo = GI {_grid :: Grid, _gridSize :: GridSize} deriving (Show)

part1 :: IO Int
part1 = solve <$> getInput

solve :: GridInfo -> Int
solve (GI grid gs) =
  length
    . nub
    . concat
    . M.elems
    . M.map (getAntiNodeLocationsForGroup gs)
    . groupByAntennae
    $ grid

groupByAntennae :: Grid -> M.Map Char [Coord]
groupByAntennae = groupMap snd fst

-- welllll I don't need the full carty prod, just need all pairs
-- OR I could do full carty prod and have getAntiNodeLocations care about ordering and produce only one (or zero) antinode(s)...
-- could also return a set so I don't have to worry too much about nub and stuff
getAntiNodeLocationsForGroup :: GridSize -> [Coord] -> [Coord]
getAntiNodeLocationsForGroup gs xs =
  nub . concat $
    [ getAntiNodeLocations gs a b
      | a <- xs,
        b <- xs,
        a /= b
    ]

getAntiNodeLocations :: GridSize -> Coord -> Coord -> [Coord]
getAntiNodeLocations gs (x1, y1) (x2, y2) =
  filter (withinBounds gs) [(x2 + dx, y2 + dy), (x1 - dx, y1 - dy)]
  where
    dx = x2 - x1
    dy = y2 - y1

withinBounds :: GridSize -> Coord -> Bool
withinBounds (GS width height) (x, y) = and [x >= 0, y >= 0, x < width, y < height]

toGrid :: [String] -> GridInfo
toGrid input = GI grid (GS width height)
  where
    height = length input
    width = length . head $ input
    grid =
      [ ((i, j), char)
        | (j, row) <- zipWithIndex input,
          (i, char) <- zipWithIndex row,
          char /= '.'
      ]
    -- based on the function of the same name in scala
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]

getInput :: IO GridInfo
getInput = toGrid . lines <$> readFile "./fixtures/input8.txt"