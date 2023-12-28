module Day23Part2 where

import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Set qualified as S
import Utils.Dijkstra (EndNode, StartNode, dijkstra)
import Utils.Grouping (groupBy', groupMap)

type Coords = (Int, Int)

type Grid = S.Set Coords

type Graph = M.Map Coords [Coords]

type ScoreMap = M.Map (Coords, Coords) Int

data Edge = Edge {from :: Coords, to :: Coords, score :: Int} deriving (Show)

part2 = do
  grid <- getGrid <$> getLines "./fixtures/input23Toy.txt"
  let startNode :: Coords = (1, 0)
  let endNode :: Coords = (21, 22) -- real end is (139, 140)
  let (graph, scoreMap) = buildGraph grid startNode endNode

  let res = solve grid startNode endNode

  print res

solve grid startNode endNode = dijkstra scoreFn neighbourGetter isEndNode startNode
  where
    scoreFn :: Coords -> Coords -> Int
    scoreFn from to = (-1) * (scoreMap M.! (from, to))
    neighbourGetter :: Coords -> [Coords]
    neighbourGetter c = graph M.! c
    isEndNode :: Coords -> Bool
    isEndNode = (== endNode)
    (graph, scoreMap) = buildGraph grid startNode endNode

buildGraph :: Grid -> Coords -> Coords -> (Graph, ScoreMap)
buildGraph grid startNode endNode =
  let allPoints = S.toList grid
      junctions = filter (isJunction grid) allPoints
      nodes = S.fromList (startNode : endNode : junctions)
      edges = findEdges grid nodes
      graph = groupMap from to edges
      scoreMap = M.map (\x -> if length x == 1 then score . head $ x else undefined) . groupBy' (\x -> (from x, to x)) $ edges
   in (graph, scoreMap)

-- remember to pass in start and end nodes...
findEdges :: Grid -> S.Set Coords -> [Edge]
findEdges g nodes = concatMap (\n -> findEdgesForNode g n (others n)) . S.toList $ nodes
  where
    others n = S.delete n nodes

findEdgesForNode :: Grid -> Coords -> S.Set Coords -> [Edge]
findEdgesForNode g c otherNodes =
  map (uncurry (Edge c) . (\n -> followPathToEdge g otherNodes (c, n, 0)))
    . getNeighbours g
    $ c

followPathToEdge :: Grid -> S.Set Coords -> (Coords, Coords, Int) -> (Coords, Int)
followPathToEdge g otherNodes (prev, current, i) =
  if S.member nextNode otherNodes
    then (nextNode, i' + 1)
    else followPathToEdge g otherNodes (current, nextNode, i')
  where
    nextNode = head . filter (/= prev) . getNeighbours g $ current
    i' = i + 1

-- could use intersections and diffs I suppose
getNeighbours :: Grid -> Coords -> [Coords]
getNeighbours g (x, y) = filter (`S.member` g) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

getGrid :: [String] -> Grid
getGrid inp = S.fromList [(x, y) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs, c /= '#']

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

isJunction :: Grid -> Coords -> Bool
isJunction g = (>= 3) . length . getNeighbours g

prettyPrintSymbolMap :: Int -> [Coords] -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[if (x, y) `elem` mp then '0' else ' ' | x <- counter] | y <- counter]
