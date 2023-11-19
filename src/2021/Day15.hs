{-# LANGUAGE TupleSections #-}

module Day15 where

import Data.Char (digitToInt)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

data Point = Point
  { x :: Int,
    y :: Int
  }
  deriving (Ord, Eq)

type Risk = Int

type AccumulatedRisk = Int

type Grid = M.Map Point Risk

type Path = [(Point, Risk)]

type UnvisitedNodes = M.Map Point (Risk, AccumulatedRisk)

type VisitedNodes = M.Map Point Risk

type UnvisitedNode = (Point, (Risk, AccumulatedRisk))

dijkstra :: (UnvisitedNodes, VisitedNodes) -> (UnvisitedNodes, VisitedNodes)
dijkstra (unvisited, visited) =
  let (currentPoint, (_, currentAccumulatedRisk)) =
        getSmallestUnvisited unvisited
      res =
        foldr
          ( uncurry M.insert
              . (\(p, (r, tr)) -> (p, (r, min tr (r + currentAccumulatedRisk))))
          )
          unvisited
          . getNeighbours unvisited
          $ currentPoint
   in (res, M.delete currentPoint visited)

getSmallestUnvisited :: UnvisitedNodes -> UnvisitedNode
getSmallestUnvisited = M.findMin

-- I suspect this will be very much a djikstra thing
-- Mark all nodes unvisited. Create a set of all the unvisited nodes called the unvisited set.
-- Assign to every node a tentative distance value: set it to zero for our initial node and to infinity for all other nodes. The tentative distance of a node v is the length of the shortest path discovered so far between the node v and the starting node. Since initially no path is known to any other vertex than the source itself (which is a path of length zero), all other tentative distances are initially set to infinity. Set the initial node as current.
-- For the current node, consider all of its unvisited neighbors and calculate their tentative distances through the current node. Compare the newly calculated tentative distance to the current assigned value and assign the smaller one. For example, if the current node A is marked with a distance of 6, and the edge connecting it with a neighbor B has length 2, then the distance to B through A will be 6 + 2 = 8. If B was previously marked with a distance greater than 8 then change it to 8. Otherwise, the current value will be kept.
-- When we are done considering all of the unvisited neighbors of the current node, mark the current node as visited and remove it from the unvisited set. A visited node will never be checked again.
-- If the destination node has been marked visited (when planning a route between two specific nodes) or if the smallest tentative distance among the nodes in the unvisited set is infinity (when planning a complete traversal; occurs when there is no connection between the initial node and remaining unvisited nodes), then stop. The algorithm has finished.
-- Otherwise, select the unvisited node that is marked with the smallest tentative distance, set it as the new current node, and go back to step 3.

getNextPoints :: Point -> [Point]
getNextPoints Point {x = x, y = y} =
  [ Point {x = x + 1, y = y},
    Point {x = x, y = y + 1},
    Point {x = x - 1, y = y},
    Point {x = x, y = y - 1}
  ]

getNeighbours :: UnvisitedNodes -> Point -> [UnvisitedNode]
getNeighbours us = mapMaybe (\k -> fmap (k,) (M.lookup k us)) . getNextPoints

-- boring parsing stuff below here
toyGrid :: Grid
toyGrid =
  parseGrid
    [ "1163751742",
      "1381373672",
      "2136511328",
      "3694931569",
      "7463417111",
      "1319128137",
      "1359912421",
      "3125421639",
      "1293138521",
      "2311944581"
    ]

parseGrid :: [[Char]] -> Grid
parseGrid grid =
  M.fromList
    [ (Point i j, digitToInt elem)
      | (i, row) <- zipWithIndex grid,
        (j, elem) <- zipWithIndex row
    ]

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]
