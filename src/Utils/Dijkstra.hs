module Utils.Dijkstra (dijkstra, StartNode, EndNode) where

-- based on my day 12 2022 implementation
-- assuming score is always an int but can probs generalise
-- also should definitely generalise using Coords to just be some note type!

import Data.Bifunctor (second)
import Data.List.Extra (find, minimumOn)
import Data.Map qualified as M (Map, adjust, alter, filter, filterWithKey, lookup, singleton, toList)
import Data.Set qualified as S (Set, map)

type StartNode nodeId = nodeId

type EndNode nodeId = nodeId

data Visitation = Visited | UnVisited deriving (Show, Eq, Ord)

type TentativeDistances nodeId = M.Map nodeId (Int, Visitation)

shouldStop :: Ord nodeId => S.Set (EndNode nodeId) -> TentativeDistances nodeId -> Bool
shouldStop endNodeCoords tds = any (\x -> fmap snd x == Just Visited) . S.map (`M.lookup` tds) $ endNodeCoords

dijkstra ::
  Ord nodeId =>
  (nodeId -> Int) -> -- scoreFn
  (nodeId -> [nodeId]) -> -- neighbourGetter
  S.Set (EndNode nodeId) ->
  StartNode nodeId ->
  Maybe (TentativeDistances nodeId)
dijkstra scoreFn neighbourGetter endNodes startNode =
  fmap (M.filterWithKey (\k (_, v) -> k `elem` endNodes && v == Visited))
    . find (shouldStop endNodes)
    . iterate (dijkstraStep scoreFn neighbourGetter)
    $ dInit
  where
    dInit = M.singleton startNode (0, UnVisited)

dijkstraStep ::
  Ord nodeId =>
  (nodeId -> Int) ->
  (nodeId -> [nodeId]) ->
  TentativeDistances nodeId ->
  TentativeDistances nodeId
dijkstraStep scoreFn neighbourGetter distanceMap =
  let (currentNodeCoords, currentNodeDist) = getCurrentNode distanceMap
      neighbours = neighbourGetter currentNodeCoords
      res = updateNeighbours scoreFn currentNodeDist distanceMap neighbours
      res' = M.adjust (\(i, _) -> (i, Visited)) currentNodeCoords res
   in res'

updateNeighbours ::
  Ord nodeId =>
  (nodeId -> Int) ->
  Int ->
  TentativeDistances nodeId ->
  [nodeId] ->
  TentativeDistances nodeId
updateNeighbours scoreFn currentNodeDistance = foldl (updateNeighbour scoreFn currentNodeDistance)

updateNeighbour ::
  Ord nodeId =>
  (nodeId -> Int) ->
  Int ->
  TentativeDistances nodeId ->
  nodeId ->
  TentativeDistances nodeId
updateNeighbour scoreFn currentNodeDistance distanceMap neighbor =
  alter' alterFn neighbor distanceMap
  where
    newScore :: Int
    newScore = currentNodeDistance + scoreFn neighbor

    alterFn :: Maybe (Int, Visitation) -> (Int, Visitation)
    alterFn (Just (i, v)) = (min i newScore, v)
    alterFn Nothing = (newScore, UnVisited)

    -- like alter but can't delete elements
    alter' :: Ord k => (Maybe a -> a) -> k -> M.Map k a -> M.Map k a
    alter' f = M.alter (Just . f)

-- I think really this should be more like a queue than having to iterate through a map each time
-- at the very least I could split up the Visited and UnVisited into separate maps...
getCurrentNode :: TentativeDistances nodeId -> (nodeId, Int)
getCurrentNode =
  minimumOn snd
    . map (second fst)
    . M.toList
    . M.filter (\(_, v) -> v == UnVisited)
