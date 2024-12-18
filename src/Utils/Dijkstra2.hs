module Utils.Dijkstra2 (dijkstra) where

-- inspired by https://github.com/GuillaumedeVolpiano/adventOfCode/blob/master/lib/Helpers/Search/Int.hs
-- essentially a combo of my old dijkstra and the above

import Control.Monad ((<=<))
import Data.Foldable (find)
import Data.Map as M
  ( Map,
    insert,
    notMember,
    singleton,
    (!),
  )
import Data.Maybe (fromJust, isJust, mapMaybe)
import Utils.PSQ as Q (PSQ, insert, minView, null, singleton)

type TentativeDistances node score = M.Map node score

data DijkstraState node score = DState
  { _tentative :: TentativeDistances node score,
    _queue :: PSQ node score,
    _foundEndNode :: Maybe (node, score)
  }

type StartNode node = node

type EndNode node = node

dijkstra ::
  (Num score, Ord score, Ord node) =>
  (node -> [(node, score)]) -> -- neighbourGetter
  (node -> Bool) -> -- isEndNode
  StartNode node -> -- startNode
  Maybe (EndNode node, score) -- I suppose this could be a few different end conditions
dijkstra neighbourGetter isEndNode startNode = res
  where
    res = _foundEndNode <=< find (isJust . _foundEndNode) . iterate (dijkstraStep neighbourGetter isEndNode) $ dInit
    dInit = DState (M.singleton startNode 0) (Q.singleton startNode 0) Nothing

dijkstraStep ::
  (Num score, Ord score, Ord node) =>
  (node -> [(node, score)]) -> -- neighbourGetter
  (node -> Bool) -> -- isEndNode
  DijkstraState node score ->
  DijkstraState node score
dijkstraStep neighbourGetter isEndNode (DState tentative queue _)
  | Q.null queue = error "queue empty!"
  | isEndNode currentNode = DState tentative queue (Just (currentNode, tentative M.! currentNode))
  | otherwise = DState tentative' queue' Nothing
  where
    (currentNode, currentNodeScore, restOfQueue) = fromJust . Q.minView $ queue
    neighboursToUpdate = mapMaybe (getNeighboursToUpdate tentative currentNodeScore) . neighbourGetter $ currentNode
    queue' = foldr (uncurry Q.insert) restOfQueue neighboursToUpdate
    tentative' = foldr (uncurry M.insert) tentative neighboursToUpdate

getNeighboursToUpdate :: (Ord node, Ord score, Num score) => TentativeDistances node score -> score -> (node, score) -> Maybe (node, score)
getNeighboursToUpdate tentative currentNodeScore (nodeId, edgeScore)
  | nodeId `M.notMember` tentative = Just (nodeId, currentNodeScore + edgeScore)
  | currentNodeScore + edgeScore < tentative M.! nodeId = Just (nodeId, currentNodeScore + edgeScore)
  | otherwise = Nothing
