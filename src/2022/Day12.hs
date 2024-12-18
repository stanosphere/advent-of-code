module Day12 where

import Data.Bifunctor (second)
import Data.Char (ord)
import qualified Data.Map as M (Map, findWithDefault, fromList, lookup, toList)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S (Set, fromList, member)
import Utils.Dijkstra (EndNode, StartNode, dijkstra)

type Coord = (Int, Int)

data Node = Node {coords :: Coord, elevation :: Elevation}

type Edges = M.Map Coord [Coord]

type Elevation = Int

type Nodes = M.Map Coord Elevation

-- 0.17 secs
-- answer: 380
part1 :: IO ()
part1 = solve startNodeSelector endNodeSelector edgeSelector
  where
    startNodeSelector = (== 'S')
    endNodeSelector = (== 'E')
    edgeSelector fromNode candidateToNode =
      if elevation candidateToNode - elevation fromNode <= 1
        then Just . coords $ candidateToNode
        else Nothing

-- 0.11 secs
-- answer: 375
part2 :: IO ()
part2 = solve startNodeSelector endNodeSelector edgeSelector
  where
    startNodeSelector = (== 'E')
    endNodeSelector = (`elem` "aS")
    edgeSelector fromNode candidateToNode =
      if elevation fromNode - elevation candidateToNode <= 1
        then Just . coords $ candidateToNode
        else Nothing

solve :: (Char -> Bool) -> (Char -> Bool) -> (Node -> Node -> Maybe Coord) -> IO ()
solve startNodeSelector endNodeSelector edgeSelector = do
  input <- getLines "./fixtures/input12.txt"
  let (nodes, startNode, endNodes) = getNodes startNodeSelector endNodeSelector input
  let edges = getEdges edgeSelector nodes
  let neighbourGetter n = map (\x -> (x, 1)) . M.findWithDefault [] n $ edges
  let isEndNode x = S.member x endNodes
  let res = dijkstra neighbourGetter isEndNode startNode
  print res

-- so much parsing stuff!
getEdges :: (Node -> Node -> Maybe Coord) -> Nodes -> Edges
getEdges shouldKeepEdge nds =
  M.fromList
    . map
      (mkEdgesForSingleNode shouldKeepEdge . getAdjacent nds . uncurry Node)
    . M.toList
    $ nds

mkEdgesForSingleNode :: (Node -> Node -> Maybe Coord) -> (Node, [Node]) -> (Coord, [Coord])
mkEdgesForSingleNode shouldKeepEdge (fromNode, candidateTos) =
  (coords fromNode, mapMaybe (shouldKeepEdge fromNode) candidateTos)

getAdjacent :: Nodes -> Node -> (Node, [Node])
getAdjacent nodeMap nd =
  let (x, y) = coords nd
      coordsToCheck =
        [ (x + 1, y),
          (x - 1, y),
          (x, y + 1),
          (x, y - 1)
        ]
   in ( nd,
        mapMaybe
          (\coords' -> fmap (Node coords') (M.lookup coords' nodeMap))
          coordsToCheck
      )

getNodes ::
  (Char -> Bool) ->
  (Char -> Bool) ->
  [String] ->
  (Nodes, StartNode Coord, S.Set (EndNode Coord))
getNodes startNodeSelector endNodeSelector input =
  let nodes =
        [ ((i, j), char)
          | (j, row) <- zip [0 ..] input,
            (i, char) <- zip [0 ..] row
        ]
      startNode = fst . head . filter (startNodeSelector . snd) $ nodes
      endNodes = S.fromList . map fst . filter (endNodeSelector . snd) $ nodes
      nodeMap = M.fromList . map (second (ord . adjustLetter)) $ nodes
   in (nodeMap, startNode, endNodes)
  where
    adjustLetter x
      | x == 'S' = 'a'
      | x == 'E' = 'z'
      | otherwise = x

-- boilerplatey things
toyInput :: [String]
toyInput = ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"]

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
