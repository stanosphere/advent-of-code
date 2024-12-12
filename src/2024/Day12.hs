module Day12 where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Utils.Grouping (frequencies)
import qualified Utils.UnionFind as UF (clusterMap, fromList, unionClusters)

-- hmmmmm maybe union find would be the way...
-- there will be an optimised way to do this that involves only storing the perimeter in my state rather than literally everything lol
-- but let's see if/how UF works first!

type Coord = (Int, Int)

type Grid = [(Coord, Char)]

type Grid' = M.Map Coord Char

type Clusters = [[Coord]]

type Edge = (Coord, Coord)

part1 :: IO Int
part1 =
  sum
    . map getClusterScore
    . toClusters
    . parseInput
    . lines
    <$> readFile "./fixtures/input12.txt"

getClusterScore :: [Coord] -> Int
getClusterScore xs = getPerimeter xs * getArea xs
  where
    -- idea here is to split each square into edges
    -- if an edge appears more than once then it can't be on the perimeter
    -- if an edge appears exactly once then it must be on the perimeter
    -- there is surely a less silly way of doing this...
    -- getPerimeter :: [Coord] -> Int
    getPerimeter :: [Coord] -> Int
    getPerimeter = M.size . M.filter (== 1) . frequencies . concatMap toEdges
      where
        toEdges :: Coord -> [Edge]
        toEdges (x, y) =
          [ ((x, y), (x + 1, y)),
            ((x, y), (x, y + 1)),
            ((x + 1, y), (x + 1, y + 1)),
            ((x, y + 1), (x + 1, y + 1))
          ]

    getArea :: [Coord] -> Int
    getArea = length

toClusters :: [(Coord, Char)] -> Clusters
toClusters grid = map S.toList . M.elems . UF.clusterMap . foldl (uncurry . UF.unionClusters) initial . getUnionsToApply $ grid
  where
    initial = UF.fromList . map fst $ grid

getUnionsToApply :: Grid -> [(Coord, Coord)]
getUnionsToApply grid = concatMap (\(c, v) -> map (sortCoords c) . neighbours grid' $ (c, v)) grid
  where
    grid' = M.fromList grid

sortCoords :: Coord -> Coord -> (Coord, Coord)
sortCoords x y = if x > y then (x, y) else (y, x)

neighbours :: Grid' -> (Coord, Char) -> [Coord]
neighbours grid ((x, y), value) =
  map fst
    . filter ((== value) . snd)
    . mapMaybe (\c -> fmap (\res -> (c, res)) . M.lookup c $ grid)
    $ [ (x, y - 1),
        (x, y + 1),
        (x + 1, y),
        (x - 1, y)
      ]

parseInput :: [String] -> Grid
parseInput grid =
  [ ((i, j), char)
    | (j, row) <- zipWithIndex grid,
      (i, char) <- zipWithIndex row
  ]
  where
    -- based on the function of the same name in scala
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]