module Day12 where

import Data.List (groupBy, sort)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Utils.Grouping (frequencies, groupMap)
import qualified Utils.UnionFind as UF (clusterMap, fromList, unionClusters)

-- hmmmmm maybe union find would be the way...
-- there will be an optimised way to do this that involves only storing the perimeter in my state rather than literally everything lol
-- but let's see if/how UF works first!

type Coord = (Int, Int)

type Grid = [(Coord, Char)]

type Grid' = M.Map Coord Char

type Clusters = [[Coord]]

type Edge = (Coord, Coord)

-- get edge count
-- so I think we can find those sub-edges which lie on the perimeter using something similar to getPerimeter
-- given these edges we can group them by x coord and sort each group by the y coord (or indeed go the other way around)
-- and then each continuous sub list constitutes an edge
-- I think this will work with shapes with holes in and all
-- as a sense check I could also try the same technique and switch x and y

getEdgeCount :: [Coord] -> Int
getEdgeCount xs = (sum . map countContiguousRegions $ horizontalEdges) + (sum . map countContiguousRegions $ verticalEdges)
  where
    edgesOnPerimeter = M.keys . M.filter (== 1) . frequencies . concatMap toEdges $ xs
    horizontalEdges =
      M.elems
        . M.map sort
        . groupMap (\((_, y1), (_, _)) -> y1) (\((x1, _), (_, _)) -> x1)
        . filter isHorizontal
        $ edgesOnPerimeter
    verticalEdges =
      M.elems
        . M.map sort
        . groupMap (\((x1, _), (_, _)) -> x1) (\((_, y1), (_, _)) -> y1)
        . filter isVertical
        $ edgesOnPerimeter

    isHorizontal :: Edge -> Bool
    isHorizontal ((_, y1), (_, y2)) = y1 == y2

    isVertical :: Edge -> Bool
    isVertical ((x1, _), (x2, _)) = x1 == x2

    toEdges :: Coord -> [Edge]
    toEdges (x, y) =
      [ ((x, y), (x + 1, y)),
        ((x, y), (x, y + 1)),
        ((x + 1, y), (x + 1, y + 1)),
        ((x, y + 1), (x + 1, y + 1))
      ]

    countContiguousRegions :: [Int] -> Int
    countContiguousRegions = length . getContiguousRegions

    getContiguousRegions :: [Int] -> [[Int]]
    getContiguousRegions = foldr folder []
      where
        folder :: Int -> [[Int]] -> [[Int]]
        folder x [] = [[x]]
        folder x ((y : ys) : yss) = if x + 1 == y then (x : y : ys) : yss else [x] : (y : ys) : yss

part1 :: IO Int
part1 =
  sum
    . map getClusterScore
    . toClusters
    . parseInput
    . lines
    <$> readFile "./fixtures/input12.txt"

part2 :: IO Int
part2 =
  sum
    . map getClusterScore'
    . toClusters
    . parseInput
    . lines
    <$> readFile "./fixtures/input12.txt"

getClusterScore' :: [Coord] -> Int
getClusterScore' xs = getEdgeCount xs * getArea xs
  where
    getArea :: [Coord] -> Int
    getArea = length

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