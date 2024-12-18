module Day15 where

import Data.Char (digitToInt)
import qualified Data.Map as M (Map, fromList, lookup)
import Data.Maybe (mapMaybe)
import Utils.Dijkstra (DijkstraResult, dijkstra)

type Coord = (Int, Int)

type Risk = Int

type AccumulatedRisk = Int

type Grid = M.Map Coord Risk

data GridSize = GS {_maxX :: Int, _maxY :: Int}

part1 :: IO (DijkstraResult Coord Risk)
part1 = uncurry solve <$> getInput

solve :: Grid -> GridSize -> DijkstraResult Coord Risk
solve grid (GS maxX maxY) = dijkstra neighbourGetter isEndNode startNode
  where
    neighbourGetter :: Coord -> [(Coord, Risk)]
    neighbourGetter = nodeToNeighbours grid

    isEndNode :: Coord -> Bool
    isEndNode coord = coord == (maxX, maxY)

    startNode :: Coord
    startNode = (0, 0)

-- nodeToNeighbours :: Grid -> Coord -> [Coord]
nodeToNeighbours :: Grid -> Coord -> [(Coord, Risk)]
nodeToNeighbours grid (x, y) = mapMaybe (`lookup'` grid) [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]

lookup' :: (Ord k) => k -> M.Map k v -> Maybe (k, v)
lookup' key mp = case M.lookup key mp of
  Nothing -> Nothing
  Just value -> Just (key, value)

getInput :: IO (Grid, GridSize)
getInput = parseGrid . lines <$> readFile "./fixtures/input15.txt"

parseGrid :: [[Char]] -> (Grid, GridSize)
parseGrid input = (M.fromList gridAsList, GS maxX maxY)
  where
    gridAsList =
      [ ((i, j), digitToInt char)
        | (j, row) <- zip [0 ..] input,
          (i, char) <- zip [0 ..] row
      ]
    maxX = maximum . map (fst . fst) $ gridAsList
    maxY = maximum . map (snd . fst) $ gridAsList

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]
