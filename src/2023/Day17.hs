module Day17 where

import Data.Char (digitToInt, intToDigit)
import Data.Foldable (traverse_)
import qualified Data.Map as M (Map, fromList, (!))
import Utils.Dijkstra (DijkstraResult, StartNode, dijkstra)

type Coords = (Int, Int)

type NodeMap = M.Map Coords Int

data Direction = U | D | L | R deriving (Eq, Ord, Show)

data Node = Node {_coords :: Coords, _dir :: Direction, _prevSteps :: Int} deriving (Eq, Ord, Show)

data GridSize = GS
  { _maxX :: Int,
    _maxY :: Int
  }
  deriving (Show)

-- inclusive
getGridSize :: [String] -> GridSize
getGridSize inp = GS ((length . head $ inp) - 1) (length inp - 1)

-- only worth considering immediate neighbours actually
getValidNeighbours :: GridSize -> Node -> [Node]
getValidNeighbours gs n = filter (withinBounds gs) (neighbourSameDirection n ++ neighboursOtherDirections n)

neighbourSameDirection :: Node -> [Node]
neighbourSameDirection (Node (x, y) U prevSteps) = [Node (x, y - 1) U (prevSteps + 1) | prevSteps < 3]
neighbourSameDirection (Node (x, y) D prevSteps) = [Node (x, y + 1) D (prevSteps + 1) | prevSteps < 3]
neighbourSameDirection (Node (x, y) L prevSteps) = [Node (x - 1, y) L (prevSteps + 1) | prevSteps < 3]
neighbourSameDirection (Node (x, y) R prevSteps) = [Node (x + 1, y) R (prevSteps + 1) | prevSteps < 3]

neighboursOtherDirections :: Node -> [Node]
neighboursOtherDirections (Node (x, y) U _) = [Node (x - 1, y) L 1, Node (x + 1, y) R 1]
neighboursOtherDirections (Node (x, y) D _) = [Node (x - 1, y) L 1, Node (x + 1, y) R 1]
neighboursOtherDirections (Node (x, y) L _) = [Node (x, y - 1) U 1, Node (x, y + 1) D 1]
neighboursOtherDirections (Node (x, y) R _) = [Node (x, y - 1) U 1, Node (x, y + 1) D 1]

withinBounds :: GridSize -> Node -> Bool
withinBounds (GS maxX maxY) (Node (x, y) _ _) =
  and
    [ x >= 0,
      x <= maxX,
      y >= 0,
      y <= maxY
    ]

getSymbolCoords :: [String] -> NodeMap
getSymbolCoords inp = M.fromList [((x, y), digitToInt c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs]

prettyPrintSymbolMap :: Int -> NodeMap -> IO ()
prettyPrintSymbolMap size mp = traverse_ putStrLn [[intToDigit (mp M.! (x, y)) | x <- counter] | y <- counter]
  where
    counter = [0 .. size]

-- this takes a good while to run
-- I think it's because I'm creating loads and loads of nodes when i don't necessarily need to
-- lke the node type has many inhabitants
part1 :: IO (DijkstraResult Node Int)
part1 = do
  x <- getLines "./fixtures/input17.txt"
  let nodeMap = getSymbolCoords x
  let gridSize = getGridSize x
  let res = solve nodeMap gridSize
  return res

-- 1238
solve :: NodeMap -> GridSize -> DijkstraResult Node Int
solve nm gs = dijkstra neighbourGetter isEndNode startNode
  where
    neighbourGetter :: Node -> [(Node, Int)]
    neighbourGetter = map (\n -> (n, scoreFn' nm n)) . getValidNeighbours gs

    isEndNode :: Node -> Bool
    isEndNode = isEndNode' gs

    startNode :: StartNode Node
    startNode = Node (0, 0) D 0 -- I guess we'll kinda need two start nodes

isEndNode' :: GridSize -> Node -> Bool
isEndNode' (GS minX minY) (Node (x, y) _ _) = x == minX && y == minY

scoreFn' :: NodeMap -> Node -> Int
scoreFn' nm (Node (x, y) _ _) = nm M.! (x, y)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)