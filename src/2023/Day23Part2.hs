module Day23Part2 where

import Data.Map qualified as M
import Data.Set qualified as S
import Utils.Grouping (groupMap)

type Coords = (Int, Int)

type Grid = S.Set Coords

type Graph = M.Map Coords [(Coords, Int)]

data Edge = Edge {from :: Coords, to :: Coords, score :: Int} deriving (Show)

-- dijkstra didn't work even on the simplified graph
-- as I said before it could well be a flaw in my implementation of dijkstra
-- I'll just do a "normal" search then

-- 6586
part2 :: IO Int
part2 = do
  grid <- getGrid <$> getLines "./fixtures/input23.txt"
  let startNode :: Coords = (1, 0)
  let endNode :: Coords = (139, 140) -- real end is (139, 140); fake is (21, 22)
  let graph = buildGraph grid startNode endNode
  let res = solve graph startNode endNode
  return res

showEdge :: Edge -> String
showEdge (Edge from to _) = show from ++ " " ++ show to

buildGraph :: Grid -> Coords -> Coords -> Graph
buildGraph g startNode endNode =
  let allPoints = S.toList g
      junctions = filter isJunction allPoints
      nodes = S.fromList (startNode : endNode : junctions)
      edges = findEdges nodes
      graph = groupMap from (\e -> (to e, score e)) edges
   in graph
  where
    isJunction :: Coords -> Bool
    isJunction = (>= 3) . length . getNeighbours

    findEdges :: S.Set Coords -> [Edge]
    findEdges nodes = concatMap (\n -> findEdgesForNode n (S.delete n nodes)) . S.toList $ nodes

    findEdgesForNode :: Coords -> S.Set Coords -> [Edge]
    findEdgesForNode c otherNodes =
      map (uncurry (Edge c) . (\n -> followPathToEdge otherNodes (c, n, 0)))
        . getNeighbours
        $ c

    followPathToEdge :: S.Set Coords -> (Coords, Coords, Int) -> (Coords, Int)
    followPathToEdge otherNodes (prev, current, i) =
      let nextNode = head . filter (/= prev) . getNeighbours $ current
          i' = i + 1
       in if S.member nextNode otherNodes
            then (nextNode, i' + 1)
            else followPathToEdge otherNodes (current, nextNode, i')

    getNeighbours :: Coords -> [Coords]
    getNeighbours (x, y) = filter (`S.member` g) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

getGrid :: [String] -> Grid
getGrid inp = S.fromList [(x, y) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs, c /= '#']

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

type Path = (Coords, S.Set Coords)

solve :: Graph -> Coords -> Coords -> Int
solve nodeMap startNode endNode = maximum . findPaths nodeMap endNode $ initialPath
  where
    initialPath = (startNode, (S.singleton startNode))

-- this is from https://github.com/GuillaumedeVolpiano/adventOfCode/blob/master/2023/days/Day23.hs
-- It's better than what I had for my path finding but I don't yet understand why
-- I've simplified it slightly (use [Int] rather than [Maybe Int] for return type)
findPaths :: Graph -> Coords -> Path -> [Int]
findPaths graph endNode (current, nodes)
  | current == endNode = [0]
  | null neighbours = []
  | otherwise =
      concatMap
        (\(node, score) -> map (score +) . findPaths graph endNode . updatePath $ node)
        neighbours
  where
    neighbours = filter (\(c, _) -> S.notMember c (nodes)) . (graph M.!) $ current
    updatePath n = (n, (S.insert n (nodes)))
