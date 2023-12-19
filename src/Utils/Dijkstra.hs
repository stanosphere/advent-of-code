module Utils.Dijkstra where

-- based on my day 12 2022 implementation
-- assuming score is always an int but can probs generalise

import Data.Bifunctor (second)
import Data.List.Extra (minimumOn)
import Data.Map qualified as M
  ( Map,
    adjust,
    alter,
    filter,
    lookup,
    singleton,
    toList,
  )
import Data.Set qualified as S (Set, map)

data Coords = Coords {x :: Int, y :: Int} deriving (Eq, Ord, Show)

type StartNode = Coords

type EndNode = Coords

data Visitation = Visited | UnVisited deriving (Show, Eq, Ord)

type TentativeDistances = M.Map Coords (Int, Visitation)

shouldStop :: S.Set EndNode -> TentativeDistances -> Bool
shouldStop endNodeCoords tds = any (\x -> fmap snd x == Just Visited) . S.map (`M.lookup` tds) $ endNodeCoords

dijkstra :: (Coords -> [Coords]) -> StartNode -> [TentativeDistances]
dijkstra neighbourGetter startNode = iterate (dijkstraStep neighbourGetter) dInit
  where
    dInit = M.singleton startNode (0, UnVisited)

dijkstraStep :: (Coords -> [Coords]) -> TentativeDistances -> TentativeDistances
dijkstraStep neighbourGetter distanceMap =
  let (currentNodeCoords, currentNodeDist) = getCurrentNode distanceMap
      neighbours = neighbourGetter currentNodeCoords
      res = updateNeighbours currentNodeDist distanceMap neighbours
      res' = M.adjust (\(i, _) -> (i, Visited)) currentNodeCoords res
   in res'

updateNeighbours :: Int -> TentativeDistances -> [Coords] -> TentativeDistances
updateNeighbours currentNodeDistance = foldl (updateNeighbour currentNodeDistance)

updateNeighbour :: Int -> TentativeDistances -> Coords -> TentativeDistances
updateNeighbour currentNodeDistance distanceMap neighbor =
  alter' (alterFn currentNodeDistance) neighbor distanceMap
  where
    alterFn :: Int -> Maybe (Int, Visitation) -> (Int, Visitation)
    alterFn currentNodeScore (Just (i, v)) = (min i (currentNodeScore + 1), v)
    alterFn currentNodeScore Nothing = (currentNodeScore + 1, UnVisited)

    -- like alter but can't delete elements
    alter' :: Ord k => (Maybe a -> a) -> k -> M.Map k a -> M.Map k a
    alter' f = M.alter (Just . f)

-- I think really this should be more like a queue than having to iterate through a map each time
-- at the very least I could split up the Visited and UnVisited into separate maps...
getCurrentNode :: TentativeDistances -> (Coords, Int)
getCurrentNode =
  minimumOn snd
    . map (second fst)
    . M.toList
    . M.filter (\(_, v) -> v == UnVisited)
