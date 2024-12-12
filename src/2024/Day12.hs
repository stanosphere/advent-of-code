module Day12 where

import Data.List (sort)
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

part1 :: IO Int
part1 = solve getClusterScore

part2 :: IO Int
part2 = solve getClusterScore'

solve :: ([Coord] -> Int) -> IO Int
solve scoreFn =
  sum
    . map scoreFn
    . toClusters
    . parseInput
    . lines
    <$> readFile "./fixtures/input12.txt"

getClusterScore' :: [Coord] -> Int
getClusterScore' xs = getEdgeCount xs * getArea xs
  where
    getArea :: [Coord] -> Int
    getArea = length

-- get edge count
-- so I think we can find those sub-edges which lie on the perimeter using something similar to getPerimeter
-- given these edges we can group them by x coord and sort each group by the y coord
-- and then each continuous sub list constitutes an edge
-- I think this will work with shapes with holes in and all
-- and then to get the other types of edge just swap x and y
-- need to be careful we don't accidentally count "Möbius" edges
-- to do this we just need to make sure that the region that we're on the perimeter of doesn't suddenly swap sides!
getEdgeCount :: [Coord] -> Int
getEdgeCount cluster = (sum . map (length . getContiguousRegionsHoriz) $ horizontalEdges) + (sum . map (length . getContiguousRegionsVert) $ verticalEdges)
  where
    edgesOnPerimeter = M.keys . M.filter (== 1) . frequencies . concatMap toEdges $ cluster

    horizontalEdges =
      M.toList
        . M.map sort
        . groupMap (\((_, y1), (_, _)) -> y1) (\((x1, _), (_, _)) -> x1)
        . filter isHorizontal
        $ edgesOnPerimeter

    verticalEdges =
      M.toList
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

    -- I think if we consider who's above and below each edge and consider this flipping to be a change in continuity then we're good...
    -- so I guess we can just use the sub-map to check these stay consistent
    -- so yeah just check the above is either this region or not...
    getContiguousRegionsHoriz :: (Int, [Int]) -> [[Int]]
    getContiguousRegionsHoriz (y, xs) = foldr append [] xs
      where
        append :: Int -> [[Int]] -> [[Int]]
        append a [] = [[a]]
        append a ((b : bs) : bss) =
          if a + 1 == b && inCluster (a, y) == inCluster (b, y)
            then (a : b : bs) : bss
            else [a] : (b : bs) : bss
        append _ _ = error "oops lol"

    getContiguousRegionsVert :: (Int, [Int]) -> [[Int]]
    getContiguousRegionsVert (x, ys) = foldr append [] ys
      where
        append :: Int -> [[Int]] -> [[Int]]
        append a [] = [[a]]
        append a ((b : bs) : bss) =
          if a + 1 == b && inCluster (x, a) == inCluster (x, b)
            then (a : b : bs) : bss
            else [a] : (b : bs) : bss
        append _ _ = error "oops lol"

    inCluster :: Coord -> Bool
    inCluster (x, y) = (x, y) `elem` cluster

getClusterScore :: [Coord] -> Int
getClusterScore xs = getPerimeter xs * getArea xs
  where
    -- idea here is to split each square into edges
    -- if an edge appears more than once then it can't be on the perimeter
    -- if an edge appears exactly once then it must be on the perimeter
    -- there is surely a less silly way of doing this...
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
toClusters grid =
  map S.toList
    . M.elems
    . UF.clusterMap
    . foldl (uncurry . UF.unionClusters) initial
    . getUnionsToApply
    $ grid
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