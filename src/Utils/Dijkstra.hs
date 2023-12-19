module Utils.Dijkstra where

-- based on my day 12 2022 implementation
-- assuming score is always an int but can probs generalise
-- also should definitely generalise using Coords to just be some note type!

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

dijkstra ::
  (Coords -> Int) ->
  (Coords -> [Coords]) ->
  StartNode ->
  [TentativeDistances]
dijkstra scoreFn neighbourGetter startNode = iterate (dijkstraStep scoreFn neighbourGetter) dInit
  where
    dInit = M.singleton startNode (0, UnVisited)

dijkstraStep ::
  (Coords -> Int) ->
  (Coords -> [Coords]) ->
  TentativeDistances ->
  TentativeDistances
dijkstraStep scoreFn neighbourGetter distanceMap =
  let (currentNodeCoords, currentNodeDist) = getCurrentNode distanceMap
      neighbours = neighbourGetter currentNodeCoords
      res = updateNeighbours scoreFn currentNodeDist distanceMap neighbours
      res' = M.adjust (\(i, _) -> (i, Visited)) currentNodeCoords res
   in res'

updateNeighbours :: (Coords -> Int) -> Int -> TentativeDistances -> [Coords] -> TentativeDistances
updateNeighbours scoreFn currentNodeDistance = foldl (updateNeighbour scoreFn currentNodeDistance)

updateNeighbour :: (Coords -> Int) -> Int -> TentativeDistances -> Coords -> TentativeDistances
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
getCurrentNode :: TentativeDistances -> (Coords, Int)
getCurrentNode =
  minimumOn snd
    . map (second fst)
    . M.toList
    . M.filter (\(_, v) -> v == UnVisited)
