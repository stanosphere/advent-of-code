module Day17 where

-- I fear this may be the one to finally finish me
-- but Dijkstra may well save me
-- I guess I just need a more "dynamic" map of where I need to go
-- -- in the past I've like made a mega map of all edges
-- -- but some edges will not be allowed to be traversed
-- I mean it's not a million miles from the laser stuff and the pipe stuff right
-- how hard can it be

-- so I think I just need to generalise that horrible implementation of Dijkstra to not take a graph but rather take a function to generate neighbors...

-- Reminder of how Dijkstra works
-- Mark all nodes unvisited. Create a set of all the unvisited nodes called the unvisited set.
-- Assign to every node a tentative distance value: set it to zero for our initial node and to infinity for all other nodes. The tentative distance of a node v is the length of the shortest path discovered so far between the node v and the starting node. Since initially no path is known to any other vertex than the source itself (which is a path of length zero), all other tentative distances are initially set to infinity. Set the initial node as current.
-- For the current node, consider all of its unvisited neighbors and calculate their tentative distances through the current node. Compare the newly calculated tentative distance to the current assigned value and assign the smaller one. For example, if the current node A is marked with a distance of 6, and the edge connecting it with a neighbor B has length 2, then the distance to B through A will be 6 + 2 = 8. If B was previously marked with a distance greater than 8 then change it to 8. Otherwise, the current value will be kept.
-- When we are done considering all of the unvisited neighbors of the current node, mark the current node as visited and remove it from the unvisited set. A visited node will never be checked again.
-- If the destination node has been marked visited (when planning a route between two specific nodes) or if the smallest tentative distance among the nodes in the unvisited set is infinity (when planning a complete traversal; occurs when there is no connection between the initial node and remaining unvisited nodes), then stop. The algorithm has finished.
-- Otherwise, select the unvisited node that is marked with the smallest tentative distance, set it as the new current node, and go back to step 3.

import Data.Char (digitToInt, intToDigit)
import Data.Foldable (traverse_)
import Data.Map qualified as M
  ( Map,
    fromList,
    (!),
  )

type Coords' = (Int, Int)

type NodeMap = M.Map Coords' Int

data State = State

-- getValidNeighbours ::

getSymbolCoords :: [String] -> NodeMap
getSymbolCoords inp = M.fromList [((x, y), digitToInt c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs]

prettyPrintSymbolMap :: Int -> NodeMap -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[intToDigit (mp M.! (x, y)) | x <- counter] | y <- counter]

part1' = do
  x <- getLines "./fixtures/input17Toy.txt"
  let nodeMap = getSymbolCoords x
  prettyPrintSymbolMap 12 nodeMap

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)