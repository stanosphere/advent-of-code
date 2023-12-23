module Day23 where

import Data.Foldable (traverse_)
import Data.Map qualified as M (Map, findWithDefault, fromList, lookup)
import Data.Maybe (mapMaybe)
import Utils.Dijkstra (EndNode, StartNode, dijkstra)

type Coords = (Int, Int)

type NodeMap = M.Map Coords Char

data Node = Node {coords :: Coords, symbol :: Char} deriving (Eq, Ord, Show)

-- I suspect I'm not properly capturing the no-backtrack condition...

getValidNeighbours :: NodeMap -> Node -> [Node]
getValidNeighbours nm (Node (x, y) '.') = mapMaybe (getNode nm) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
getValidNeighbours nm (Node (x, y) '<') = mapMaybe (getNode nm) [(x - 1, y)]
getValidNeighbours nm (Node (x, y) '>') = mapMaybe (getNode nm) [(x + 1, y)]
getValidNeighbours nm (Node (x, y) '^') = mapMaybe (getNode nm) [(x, y - 1)]
getValidNeighbours nm (Node (x, y) 'v') = mapMaybe (getNode nm) [(x, y + 1)]
getValidNeighbours _ _ = undefined

getNode :: NodeMap -> Coords -> Maybe Node
getNode nm coords = case M.lookup coords nm of
  Just c -> Just (Node coords c)
  Nothing -> Nothing

getSymbolCoords :: [String] -> NodeMap
getSymbolCoords inp = M.fromList [((x, y), c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs, c /= '#']

prettyPrintSymbolMap :: Int -> NodeMap -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[M.findWithDefault ' ' (x, y) mp | x <- counter] | y <- counter]

part1 = do
  x <- getLines "./fixtures/input23Toy.txt"
  let nodeMap = getSymbolCoords x
  let res = solve nodeMap
  print res

solve :: NodeMap -> Maybe (EndNode Node, Int)
solve nm = dijkstra scoreFn neighbourGetter isEndNode startNode
  where
    scoreFn :: Node -> Int
    scoreFn = const (-1)
    neighbourGetter :: Node -> [Node]
    neighbourGetter = getValidNeighbours nm
    isEndNode :: Node -> Bool
    isEndNode = isEndNode'
    startNode :: StartNode Node
    startNode = Node (1, 0) '.'

isEndNode' :: Node -> Bool
isEndNode' (Node (x, y) _) = x == 21 && y == 22

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)