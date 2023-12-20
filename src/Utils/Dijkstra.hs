{-# LANGUAGE ImportQualifiedPost #-}

module Utils.Dijkstra (dijkstra, StartNode, EndNode, DijkstraState) where

-- based on my day 12 2022 implementation and heavily refactored + improved
-- assuming score is always an int but can probs generalise

-- note: `<=<` is basically the same as `.` but for monadic functions

import Control.Monad ((<=<))
import Data.List.Extra (find, minimumOn)
import Data.Map qualified as M (Map, alter, delete, empty, insert, notMember, singleton, toList)
import Data.Maybe (isJust)
import Data.Set qualified as S (Set, member)

type StartNode nodeId = nodeId

type EndNode nodeId = nodeId

data DijkstraState nodeId = DState
  { visited :: FinalisedDistances nodeId,
    unVisited :: TentativeDistances nodeId,
    foundEndNode :: Maybe (nodeId, Int)
  }

type TentativeDistances nodeId = M.Map nodeId Int

type FinalisedDistances nodeId = M.Map nodeId Int

dijkstra ::
  Ord nodeId =>
  (nodeId -> Int) -> -- scoreFn
  (nodeId -> [nodeId]) -> -- neighbourGetter
  S.Set (EndNode nodeId) ->
  StartNode nodeId ->
  Maybe (EndNode nodeId, Int)
dijkstra scoreFn neighbourGetter endNodes startNode = res
  where
    res = foundEndNode <=< find (isJust . foundEndNode) . iterate (dijkstraStep scoreFn neighbourGetter endNodes) $ dInit
    dInit = DState M.empty (M.singleton startNode 0) Nothing

dijkstraStep ::
  Ord nodeId =>
  (nodeId -> Int) ->
  (nodeId -> [nodeId]) ->
  S.Set (EndNode nodeId) ->
  DijkstraState nodeId ->
  DijkstraState nodeId
dijkstraStep scoreFn neighbourGetter endNodes (DState visited unVisited _) = DState visited' unVisited' foundEndNode'
  where
    (currentNodeId, currentNodeDist) = getCurrentNode unVisited
    neighbours = filter (`M.notMember` visited) . neighbourGetter $ currentNodeId
    unVisited' = M.delete currentNodeId . updateNeighbours scoreFn currentNodeDist unVisited $ neighbours
    visited' = M.insert currentNodeId currentNodeDist visited
    foundEndNode' = if S.member currentNodeId endNodes then Just (currentNodeId, currentNodeDist) else Nothing

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
updateNeighbour scoreFn currentNodeDistance distanceMap neighbor = M.alter alterFn neighbor distanceMap
  where
    newScore :: Int
    newScore = currentNodeDistance + scoreFn neighbor

    alterFn :: Maybe Int -> Maybe Int
    alterFn (Just oldScore) = Just (min oldScore newScore)
    alterFn Nothing = Just newScore

-- I think really this should be more like a queue than having to iterate through a map each time
-- I think what we really want is something like scala's Sorted set https://www.scala-lang.org/api/2.13.4/scala/collection/SortedSet.html
-- But for day 12 2022 each part finishes in < 0.2 secs so I think we're good
getCurrentNode :: TentativeDistances nodeId -> (nodeId, Int)
getCurrentNode = minimumOn snd . M.toList