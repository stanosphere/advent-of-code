module Utils.Dijkstra2 (dijkstra, DijkstraResult (QueueEmptied, FoundEndNode)) where

-- inspired by https://github.com/GuillaumedeVolpiano/adventOfCode/blob/master/lib/Helpers/Search/Int.hs
-- essentially a combo of my old dijkstra and the above

import Data.Map as M
  ( Map,
    insert,
    notMember,
    singleton,
    (!),
  )
import Data.Maybe (fromJust, mapMaybe)
import Utils.PSQ as Q (PSQ, insert, minView, null, singleton)

type TentativeDistances node score = M.Map node score

data DijkstraResult node score
  = FoundEndNode (node, score)
  | QueueEmptied -- maybe AllNodesVisited would be a more accurate description
  deriving (Show, Eq)

data DijkstraState node score = DState
  { _tentative :: TentativeDistances node score,
    _queue :: PSQ node score
  }

type StartNode node = node

type EndNode node = node

dijkstra ::
  (Num score, Ord score, Ord node) =>
  (node -> [(node, score)]) -> -- neighbourGetter
  (EndNode node -> Bool) -> -- isEndNode
  StartNode node -> -- startNode
  DijkstraResult node score
dijkstra neighbourGetter isEndNode startNode = dijkstraRec neighbourGetter isEndNode dInit
  where
    dInit = DState (M.singleton startNode 0) (Q.singleton startNode 0)

dijkstraRec ::
  (Num score, Ord score, Ord node) =>
  (node -> [(node, score)]) -> -- neighbourGetter
  (node -> Bool) -> -- isEndNode
  DijkstraState node score ->
  DijkstraResult node score
dijkstraRec neighbourGetter isEndNode (DState tentative queue)
  | Q.null queue = QueueEmptied
  | isEndNode currentNode = FoundEndNode (currentNode, tentative M.! currentNode)
  | otherwise = dijkstraRec neighbourGetter isEndNode (DState tentative' queue')
  where
    (currentNode, currentNodeScore, restOfQueue) = fromJust . Q.minView $ queue
    neighboursToUpdate = mapMaybe (getNeighboursToUpdate tentative currentNodeScore) . neighbourGetter $ currentNode
    queue' = foldr (uncurry Q.insert) restOfQueue neighboursToUpdate
    tentative' = foldr (uncurry M.insert) tentative neighboursToUpdate

-- could do the above in a single fold if we _really_ wanted to but it's prettier how it is
-- state' = foldr (\(node, score) (DState m q) -> DState (M.insert node score m) (Q.insert node score q)) (DState tentative restOfQueue) neighboursToUpdate

getNeighboursToUpdate ::
  (Ord node, Ord score, Num score) =>
  TentativeDistances node score ->
  score ->
  (node, score) ->
  Maybe (node, score)
getNeighboursToUpdate tentative currentNodeScore (nodeId, edgeScore)
  | nodeId `M.notMember` tentative = Just (nodeId, currentNodeScore + edgeScore)
  | currentNodeScore + edgeScore < tentative M.! nodeId = Just (nodeId, currentNodeScore + edgeScore)
  | otherwise = Nothing
