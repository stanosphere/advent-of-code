{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Utils.DijkstraOld (dijkstra, dijkstraVisitAll, StartNode, EndNode, DijkstraState (_visited, _unVisited)) where

-- based on my day 12 2022 implementation and heavily refactored + improved
-- assuming score is always an int but can probs generalise
-- should probably rename *dist* to *score* at some point...

-- note: `<=<` is basically the same as `.` but for monadic functions

import Control.Monad ((<=<))
import Data.List (foldl')
import Data.List.Extra (find, minimumOn)
import qualified Data.Map as M (Map, alter, delete, empty, insert, notMember, null, singleton, toList)
import Data.Maybe (isJust)

type StartNode nodeId = nodeId

type EndNode nodeId = nodeId

data DijkstraState nodeId = DState
  { _visited :: FinalisedDistances nodeId,
    _unVisited :: TentativeDistances nodeId,
    _foundEndNode :: Maybe (nodeId, Int)
  }
  deriving (Show)

-- as per the comment above `getCurrentNode` this should really be a sorted map of some kind
type TentativeDistances nodeId = M.Map nodeId Int

type FinalisedDistances nodeId = M.Map nodeId Int

-- in scala I'd make this a trait where you have to specify the functions
-- maybe this can be done in Haskell by making it a class rather than a function?
dijkstra ::
  (Ord nodeId) =>
  (nodeId -> nodeId -> Int) -> -- scoreFn
  (nodeId -> [nodeId]) -> -- neighbourGetter
  (nodeId -> Bool) -> -- end node check
  StartNode nodeId ->
  Maybe (EndNode nodeId, Int)
dijkstra scoreFn neighbourGetter isEndNode startNode = res
  where
    res = _foundEndNode <=< find (isJust . _foundEndNode) . iterate (dijkstraStep scoreFn neighbourGetter isEndNode) $ dInit
    dInit = DState M.empty (M.singleton startNode 0) Nothing

-- version of dijkstra that visits all nodes
dijkstraVisitAll ::
  (Ord nodeId) =>
  (nodeId -> nodeId -> Int) -> -- scoreFn
  (nodeId -> [nodeId]) -> -- neighbourGetter
  StartNode nodeId ->
  Maybe (DijkstraState nodeId)
dijkstraVisitAll scoreFn neighbourGetter startNode = res
  where
    res = find (M.null . _unVisited) . iterate (dijkstraStep scoreFn neighbourGetter (const False)) $ dInit
    dInit = DState M.empty (M.singleton startNode 0) Nothing

dijkstraStep ::
  (Ord nodeId) =>
  (nodeId -> nodeId -> Int) ->
  (nodeId -> [nodeId]) ->
  (nodeId -> Bool) ->
  DijkstraState nodeId ->
  DijkstraState nodeId
dijkstraStep scoreFn neighbourGetter isEndNode (DState visited unVisited _) = DState visited' unVisited' foundEndNode'
  where
    (currentNodeId, currentNodeDist) = getCurrentNode unVisited
    neighbours = filter (`M.notMember` visited) . neighbourGetter $ currentNodeId
    neighboursWithScores = map (\n -> (n, currentNodeDist + scoreFn currentNodeId n)) neighbours
    unVisited' = M.delete currentNodeId . updateNeighbours unVisited $ neighboursWithScores
    visited' = M.insert currentNodeId currentNodeDist visited
    foundEndNode' = if isEndNode currentNodeId then Just (currentNodeId, currentNodeDist) else Nothing

updateNeighbours ::
  (Ord nodeId) =>
  TentativeDistances nodeId ->
  [(nodeId, Int)] ->
  TentativeDistances nodeId
updateNeighbours = foldl' updateNeighbour

updateNeighbour ::
  (Ord nodeId) =>
  TentativeDistances nodeId ->
  (nodeId, Int) ->
  TentativeDistances nodeId
updateNeighbour distanceMap (neighbor, newScore) = M.alter alterFn neighbor distanceMap
  where
    alterFn :: Maybe Int -> Maybe Int
    alterFn (Just oldScore) = Just (min oldScore newScore)
    alterFn Nothing = Just newScore

-- I think really this should be more like a queue than having to iterate through a map each time
-- I think what we really want is something like scala's Sorted set https://www.scala-lang.org/api/2.13.4/scala/collection/SortedSet.html
-- But for day 12 2022 each part finishes in < 0.2 secs so I think we're good
getCurrentNode :: TentativeDistances nodeId -> (nodeId, Int)
getCurrentNode = minimumOn snd . M.toList