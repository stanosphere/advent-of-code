module Day17 where

-- I fear this may be the one to finally finish me
-- but Dijkstra may well save me
-- I guess I just need a more "dynamic" map of where I need to go
-- -- in the past I've like made a mega map of all edges
-- -- but some edges will not be allowed to be traversed
-- I mean it's not a million miles from the laser stuff and the pipe stuff right
-- how hard can it be

import Data.Char (digitToInt, intToDigit)
import Data.Foldable (traverse_)
import Data.Map qualified as M
  ( Map,
    fromList,
    (!),
  )
import Utils.Dijkstra (EndNode, StartNode, dijkstra)

type Coords = (Int, Int)

type NodeMap = M.Map Coords Int

data Direction = N | E | S | W

data Node = Node {coords :: Coords, dir :: Direction, prevSteps :: Int}

data State = State

getValidNeighbours :: NodeMap -> Node -> [Node]
getValidNeighbours nm n = [n]

getSymbolCoords :: [String] -> NodeMap
getSymbolCoords inp = M.fromList [((x, y), digitToInt c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs]

prettyPrintSymbolMap :: Int -> NodeMap -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[intToDigit (mp M.! (x, y)) | x <- counter] | y <- counter]

part1 = do
  x <- getLines "./fixtures/input17Toy.txt"
  let nodeMap = getSymbolCoords x
  prettyPrintSymbolMap 12 nodeMap

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)