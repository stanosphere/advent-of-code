module Utils.Dijkstra where

-- based on my day 12 2022 implementation

import Data.Bifunctor (second)
import Data.Char (ord)
import Data.Map qualified as M
  ( Map,
    adjust,
    alter,
    filter,
    findWithDefault,
    fromList,
    lookup,
    mapWithKey,
    singleton,
    toList,
    (!),
  )
import Data.Maybe (mapMaybe)
import Data.Set qualified as S (Set, fromList, map)

data Coords = Coords {x :: Int, y :: Int} deriving (Eq, Ord, Show)

data Node = Node {coords :: Coords, elevation :: Elevation}

type Elevation = Int

type Nodes = M.Map Coords Elevation

type Edges = M.Map Coords [Coords]

type StartNode = Coords

type EndNode = Coords

data Visitation = Visited | UnVisited deriving (Show, Eq, Ord)

type TentativeDistances = M.Map Coords (Int, Visitation)

shouldStop :: S.Set EndNode -> TentativeDistances -> Bool
shouldStop endNodeCoords tds = all (\x -> fmap snd x == Just Visited) . S.map (`M.lookup` tds) $ endNodeCoords

dijkstra :: (StartNode, Edges) -> [TentativeDistances]
dijkstra (startNode, edges) =
  let dInit = mkDijkstraInit startNode edges
   in iterate (dijkstraStep edges) dInit

-- ok so I think this can actually just start as empty map which we slowly add to over time
mkDijkstraInit :: StartNode -> Edges -> TentativeDistances
mkDijkstraInit startNode _ = M.singleton startNode (0, UnVisited)

dijkstraStep :: Edges -> TentativeDistances -> TentativeDistances
dijkstraStep edges distanceMap =
  let (currentNodeCoords, currentNodeDist) = getCurrentNode distanceMap
      neighbours = M.findWithDefault [] currentNodeCoords edges
      res = updateNeighbours currentNodeDist distanceMap neighbours
      res' = M.adjust (\(i, _) -> (i, Visited)) currentNodeCoords res
   in res'

updateNeighbours :: Int -> TentativeDistances -> [Coords] -> TentativeDistances
updateNeighbours currentNodeDistance =
  foldl (updateNeighbour currentNodeDistance)

updateNeighbour :: Int -> TentativeDistances -> Coords -> TentativeDistances
updateNeighbour currentNodeDistance distanceMap neighbor =
  alter' (alterFn currentNodeDistance) neighbor distanceMap
  where
    alterFn :: Int -> Maybe (Int, Visitation) -> (Int, Visitation)
    alterFn currentNodeDistance (Just (i, v)) = (min i (currentNodeDistance + 1), v)
    alterFn currentNodeDistance Nothing = ((currentNodeDistance + 1), UnVisited)

    -- like alter but can't delete elements
    alter' :: Ord k => (Maybe a -> a) -> k -> M.Map k a -> M.Map k a
    alter' f = M.alter (Just . f)

-- kinda feel like one could use minBy or something like that
getCurrentNode :: TentativeDistances -> (Coords, Int)
getCurrentNode =
  foldr
    (\(k1, v1) -> \(k2, v2) -> if v1 < v2 then (k1, v1) else (k2, v2))
    (Coords (-1) (-1), maxInt)
    . map (\(k, v) -> (k, fst v))
    . M.toList
    . M.filter (\(_, v) -> v == UnVisited)

maxInt :: Int
maxInt = maxBound

-- so much parsing stuff
-- getEdges :: (Node -> Node -> Maybe Coords) -> Nodes -> Edges
-- getEdges shouldKeepEdge nds =
--   M.fromList
--     . map
--       (mkEdgesForSingleNode shouldKeepEdge . getAdjacent nds . uncurry Node)
--     . M.toList
--     $ nds

-- mkEdgesForSingleNode ::
--   (Node -> Node -> Maybe Coords) -> (Node, [Node]) -> (Coords, [Coords])
-- mkEdgesForSingleNode shouldKeepEdge (fromNode, candidateTos) =
--   (coords fromNode, mapMaybe (shouldKeepEdge fromNode) candidateTos)

-- getAdjacent :: Nodes -> Node -> (Node, [Node])
-- getAdjacent nodeMap nd =
--   let ndCoords = coords nd
--       coordsToCheck =
--         [ ndCoords {x = 1 + x ndCoords},
--           ndCoords {x = (-1) + x ndCoords},
--           ndCoords {y = 1 + y ndCoords},
--           ndCoords {y = (-1) + y ndCoords}
--         ]
--    in ( nd,
--         mapMaybe
--           (\coords' -> fmap (Node coords') (M.lookup coords' nodeMap))
--           coordsToCheck
--       )

-- getNodes ::
--   (Char -> Bool) ->
--   (Char -> Bool) ->
--   [String] ->
--   (Nodes, StartNode, S.Set EndNode)
-- getNodes startNodeSelector endNodeSelector input =
--   let nodes =
--         [ (Coords i j, x)
--           | (j, row) <- zip [0 ..] input,
--             (i, x) <- zip [0 ..] row
--         ]
--       startNode = fst . head . filter (startNodeSelector . snd) $ nodes
--       endNodes = S.fromList . map fst . filter (endNodeSelector . snd) $ nodes
--       nodeMap = M.fromList . map (second (ord . adjustLetter)) $ nodes
--    in (nodeMap, startNode, endNodes)
--   where
--     adjustLetter x
--       | x == 'S' = 'a'
--       | x == 'E' = 'z'
--       | otherwise = x
