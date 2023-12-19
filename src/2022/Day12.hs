module Day12 where

import Data.Bifunctor (second)
import Data.Char (ord)
import Data.Foldable (traverse_)
import Data.Map qualified as M
  ( Map,
    adjust,
    filter,
    filterWithKey,
    findWithDefault,
    fromList,
    lookup,
    mapWithKey,
    toList,
    (!),
  )
import Data.Maybe (mapMaybe)
import Data.Set qualified as S (Set, fromList, map)
import Utils.Dijkstra

-- data Coords = Coords {x :: Int, y :: Int} deriving (Eq, Ord, Show)

-- data Node = Node {coords :: Coords, elevation :: Elevation}

-- type Elevation = Int

-- type Nodes = M.Map Coords Elevation

-- type Edges = M.Map Coords [Coords]

-- type StartNode = Coords

-- type EndNode = Coords

-- data Visitation = Visited | UnVisited deriving (Show, Eq, Ord)

-- type TentativeDistances = M.Map Coords (Int, Visitation)

-- 6.38 secs
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

part2 :: IO ()
part2 = solve startNodeSelector endNodeSelector edgeSelector
  where
    startNodeSelector = (== 'E')
    endNodeSelector = (`elem` "aS")
    edgeSelector fromNode candidateToNode =
      if elevation fromNode - elevation candidateToNode <= 1
        then Just . coords $ candidateToNode
        else Nothing

solve ::
  (Char -> Bool) -> (Char -> Bool) -> (Node -> Node -> Maybe Coords) -> IO ()
solve startNodeSelector endNodeSelector edgeSelector = do
  input <- getLines "./fixtures/input12.txt"
  let (nodes, startNode, endNodes) =
        getNodes startNodeSelector endNodeSelector input
  let edges = getEdges edgeSelector nodes
  let res =
        M.filterWithKey (\k (i, v) -> k `elem` endNodes && v == Visited)
          . head
          . dropWhile (shouldContinue endNodes)
          . dijkstra
          $ (startNode, edges)
  traverse_ print . M.toList $ res

-- shouldContinue :: S.Set EndNode -> TentativeDistances -> Bool
-- shouldContinue coordss tds = not . any (\(_, v) -> v == Visited) . S.map (tds M.!) $ coordss

-- dijkstra :: (StartNode, Edges) -> [TentativeDistances]
-- dijkstra (startNode, edges) =
--   let dInit = mkDijkstraInit startNode edges
--    in iterate (dijkstraStep edges) dInit

-- mkDijkstraInit :: StartNode -> Edges -> TentativeDistances
-- mkDijkstraInit startNode =
--   M.mapWithKey
--     (\k _ -> if k == startNode then (0, UnVisited) else (maxInt, UnVisited))

-- dijkstraStep :: Edges -> TentativeDistances -> TentativeDistances
-- dijkstraStep edges distanceMap =
--   let (currentNodeCoords, currentNodeDist) = getCurrentNode distanceMap
--       neighbours = M.findWithDefault [] currentNodeCoords edges
--       res = updateNeighbours currentNodeDist distanceMap neighbours
--       res' = M.adjust (\(i, _) -> (i, Visited)) currentNodeCoords res
--    in res'

-- updateNeighbours :: Int -> TentativeDistances -> [Coords] -> TentativeDistances
-- updateNeighbours currentNodeDistance =
--   foldl (updateNeighbour currentNodeDistance)

-- updateNeighbour :: Int -> TentativeDistances -> Coords -> TentativeDistances
-- updateNeighbour currentNodeDistance distanceMap neighbor =
--   M.adjust
--     (\(i, v) -> (min i (currentNodeDistance + 1), v))
--     neighbor
--     distanceMap

-- getCurrentNode :: TentativeDistances -> (Coords, Int)
-- getCurrentNode =
--   foldr
--     (\(k1, v1) -> \(k2, v2) -> if v1 < v2 then (k1, v1) else (k2, v2))
--     (Coords (-1) (-1), maxInt)
--     . map (\(k, v) -> (k, fst v))
--     . M.toList
--     . M.filter (\(_, v) -> v == UnVisited)

maxInt :: Int
maxInt = maxBound

-- so much parsing stuff!
getEdges :: (Node -> Node -> Maybe Coords) -> Nodes -> Edges
getEdges shouldKeepEdge nds =
  M.fromList
    . map
      (mkEdgesForSingleNode shouldKeepEdge . getAdjacent nds . uncurry Node)
    . M.toList
    $ nds

mkEdgesForSingleNode ::
  (Node -> Node -> Maybe Coords) -> (Node, [Node]) -> (Coords, [Coords])
mkEdgesForSingleNode shouldKeepEdge (fromNode, candidateTos) =
  (coords fromNode, mapMaybe (shouldKeepEdge fromNode) candidateTos)

getAdjacent :: Nodes -> Node -> (Node, [Node])
getAdjacent nodeMap nd =
  let ndCoords = coords nd
      coordsToCheck =
        [ ndCoords {x = 1 + x ndCoords},
          ndCoords {x = (-1) + x ndCoords},
          ndCoords {y = 1 + y ndCoords},
          ndCoords {y = (-1) + y ndCoords}
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
  (Nodes, StartNode, S.Set EndNode)
getNodes startNodeSelector endNodeSelector input =
  let nodes =
        [ (Coords i j, x)
          | (j, row) <- zip [0 ..] input,
            (i, x) <- zip [0 ..] row
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
