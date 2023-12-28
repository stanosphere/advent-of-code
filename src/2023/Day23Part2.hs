module Day23Part2 where

import Data.Foldable (find, traverse_)
import Data.Map qualified as M
import Data.Set qualified as S
import Utils.Grouping (groupBy', groupMap)

type Coords = (Int, Int)

type Grid = S.Set Coords

type Graph = M.Map Coords [(Coords, Int)]

data Edge = Edge {from :: Coords, to :: Coords, score :: Int} deriving (Show)

-- dijkstra didn't work even on the simplified graph
-- as I said before it could well be a flaw in my implementation of dijkstra
-- I'll just do a "normal" search then

-- 2738 is too low
part2 = do
  grid <- getGrid <$> getLines "./fixtures/input23Toy.txt"
  let startNode :: Coords = (1, 0)
  let endNode :: Coords = (21, 22) -- real end is (139, 140)
  let graph = buildGraph grid startNode endNode
  let res = solve graph endNode
  print res

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

data Path = Path {current :: Coords, rest :: S.Set Coords, size :: Int} deriving (Show)

data State = State {unfinished :: [Path], finished :: [Path]} deriving (Show)

-- -- solve :: Graph -> Coords -> Maybe Int
-- solve grid endNode =
--   iterate (step grid endNode) $ initState
--   where
--     initState = State [Path (1, 0) S.empty 0] []

solve :: Graph -> Coords -> Maybe Int
solve grid endNode =
  fmap (maximum . map (size) . finished) . find (null . unfinished) . iterate (step grid endNode) $ initState
  where
    initState = State [Path (1, 0) S.empty 0] []

step :: Graph -> Coords -> State -> State
step g endCoords (State unfinished finished) =
  let stepped = concatMap (stepPath g) unfinished
      newFinished = filter (\p -> current p == endCoords) stepped
      newUnfinished = filter (\p -> current p /= endCoords) stepped
   in State newUnfinished (finished ++ newFinished)

stepPath :: Graph -> Path -> [Path]
stepPath g p = map (appendPath p) . filter (\(c, _) -> S.notMember c (rest p)) . (g M.!) . current $ p

appendPath :: Path -> (Coords, Int) -> Path
appendPath (Path prev rest len) (c, i) = Path c (S.insert prev rest) (len + i)
