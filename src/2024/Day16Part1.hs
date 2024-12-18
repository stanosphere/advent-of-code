module Day16Part1 where

import qualified Data.Map as M
import qualified Data.Set as S
import Utils.Dijkstra (DijkstraResult, dijkstra)

data Direction = N | E | S | W deriving (Show, Eq, Ord)

type Coord = (Int, Int)

data Node = Nd {_position :: Coord, _orientation :: Direction} deriving (Eq, Show, Ord)

type Result = DijkstraResult Node Int

part1 :: IO Result
part1 = do
  (coords, start, end) <- parseInput . lines <$> readFile "./fixtures/input16.txt"
  return (solve coords start end)

solve :: [Coord] -> Coord -> Coord -> Result
solve coords startCoord endCoord =
  dijkstra neighbourGetter isEndNode startNode
  where
    neighbourGetter :: Node -> [(Node, Int)]
    neighbourGetter n = neighbourMap M.! n

    isEndNode :: Node -> Bool
    isEndNode (Nd coord _) = coord == endCoord

    startNode :: Node
    startNode = Nd startCoord E

    coordSet = S.fromList coords
    neighbourMap =
      M.fromList
        . map (\n -> (n, nodeToNeighbours coordSet $ n))
        . coordsToNodes
        $ coords

coordsToNodes :: [Coord] -> [Node]
coordsToNodes = concatMap (\c -> map (Nd c) [N, E, S, W])

nodeToNeighbours :: S.Set Coord -> Node -> [(Node, Int)]
nodeToNeighbours s nd = (map (\x -> (x, 1000)) . otherOrientations $ nd) ++ (map (\x -> (x, 1)) . adjacent $ nd)
  where
    otherOrientations :: Node -> [Node]
    otherOrientations (Nd (x, y) N) = map (Nd (x, y)) [E, S, W]
    otherOrientations (Nd (x, y) E) = map (Nd (x, y)) [N, S, W]
    otherOrientations (Nd (x, y) S) = map (Nd (x, y)) [N, E, W]
    otherOrientations (Nd (x, y) W) = map (Nd (x, y)) [N, E, S]

    adjacent :: Node -> [Node]
    adjacent (Nd (x, y) N) = [Nd (x, y - 1) N | S.member (x, y - 1) s]
    adjacent (Nd (x, y) E) = [Nd (x + 1, y) E | S.member (x + 1, y) s]
    adjacent (Nd (x, y) S) = [Nd (x, y + 1) S | S.member (x, y + 1) s]
    adjacent (Nd (x, y) W) = [Nd (x - 1, y) W | S.member (x - 1, y) s]

parseInput :: [String] -> ([Coord], Coord, Coord)
parseInput xs = (map fst g, start, end)
  where
    g = [((i, j), char) | (j, row) <- zip [0 ..] xs, (i, char) <- zip [0 ..] row, char `elem` "ES."]
    start = fst . head . filter ((== 'S') . snd) $ g
    end = fst . head . filter ((== 'E') . snd) $ g
