module Day17Part2 where

import Data.Char (digitToInt, intToDigit)
import Data.Foldable (traverse_)
import qualified Data.Map as M (Map, fromList, (!))
import Utils.Dijkstra (EndNode, StartNode, dijkstra)

type Coords = (Int, Int)

type NodeMap = M.Map Coords Int

data Direction = U | D | L | R deriving (Eq, Ord, Show)

data Node = Node {coords :: Coords, dir :: Direction, prevSteps :: Int} deriving (Eq, Ord, Show)

data GridSize = GS
  { maxX :: Int,
    maxY :: Int
  }
  deriving (Show)

data State = State

-- inclusive
getGridSize :: [String] -> GridSize
getGridSize inp = GS ((length . head $ inp) - 1) (length inp - 1)

-- only worth considering immediate neighbours actually
getValidNeighbours :: GridSize -> Node -> [Node]
getValidNeighbours gs n =
  if coords n == (0, 0)
    then filter (withinBounds gs) (neighboursOtherDirections (Node (0, 0) D 0) ++ neighboursOtherDirections (Node (0, 0) R 0))
    else filter (withinBounds gs) . getValidNeighbours' $ n

getValidNeighbours' :: Node -> [Node]
getValidNeighbours' n = neighbourSameDirection n ++ neighboursOtherDirections n

neighbourSameDirection :: Node -> [Node]
neighbourSameDirection (Node (x, y) U prevSteps) = [Node (x, y - 1) U (prevSteps + 1) | prevSteps < 10]
neighbourSameDirection (Node (x, y) D prevSteps) = [Node (x, y + 1) D (prevSteps + 1) | prevSteps < 10]
neighbourSameDirection (Node (x, y) L prevSteps) = [Node (x - 1, y) L (prevSteps + 1) | prevSteps < 10]
neighbourSameDirection (Node (x, y) R prevSteps) = [Node (x + 1, y) R (prevSteps + 1) | prevSteps < 10]

-- basically teleports it
neighboursOtherDirections :: Node -> [Node]
neighboursOtherDirections (Node (x, y) U _) = [Node (x - 4, y) L 4, Node (x + 4, y) R 4]
neighboursOtherDirections (Node (x, y) D _) = [Node (x - 4, y) L 4, Node (x + 4, y) R 4]
neighboursOtherDirections (Node (x, y) L _) = [Node (x, y - 4) U 4, Node (x, y + 4) D 4]
neighboursOtherDirections (Node (x, y) R _) = [Node (x, y - 4) U 4, Node (x, y + 4) D 4]

withinBounds :: GridSize -> Node -> Bool
withinBounds (GS maxX maxY) (Node (x, y) _ _) = x >= 0 && x <= maxX && y >= 0 && y <= maxY

getSymbolCoords :: [String] -> NodeMap
getSymbolCoords inp = M.fromList [((x, y), digitToInt c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs]

prettyPrintSymbolMap :: Int -> NodeMap -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[intToDigit (mp M.! (x, y)) | x <- counter] | y <- counter]

-- 1362
-- takes about a minute to run !!!
part2 :: IO (Maybe (EndNode Node, Int))
part2 = do
  x <- getLines "./fixtures/input17.txt"
  let nodeMap = getSymbolCoords x
  let gridSize = getGridSize x
  let res = solve nodeMap gridSize
  return res

solve :: NodeMap -> GridSize -> Maybe (EndNode Node, Int)
solve nm gs = dijkstra scoreFn neighbourGetter isEndNode startNode
  where
    scoreFn :: Node -> Node -> Int
    scoreFn _ = scoreFn' nm
    neighbourGetter :: Node -> [Node]
    neighbourGetter = getValidNeighbours gs
    isEndNode :: Node -> Bool
    isEndNode = isEndNode' gs
    startNode :: StartNode Node
    startNode = Node (0, 0) R 0

isEndNode' :: GridSize -> Node -> Bool
isEndNode' (GS minX minY) (Node (x, y) _ _) = x == minX && y == minY

scoreFn' :: NodeMap -> Node -> Int
scoreFn' nm n@(Node _ _ prevSteps) =
  if prevSteps == 4
    then scoreFnForTeleport nm n
    else scoreFnForCoords nm (coords n)

scoreFnForCoords :: NodeMap -> Coords -> Int
scoreFnForCoords nm cs = nm M.! cs

scoreFnForTeleport :: NodeMap -> Node -> Int
scoreFnForTeleport nm (Node (x, y) R _) = sum . map (\off -> scoreFnForCoords nm (x - off, y)) $ [0 .. 3]
scoreFnForTeleport nm (Node (x, y) L _) = sum . map (\off -> scoreFnForCoords nm (x + off, y)) $ [0 .. 3]
scoreFnForTeleport nm (Node (x, y) U _) = sum . map (\off -> scoreFnForCoords nm (x, y + off)) $ [0 .. 3]
scoreFnForTeleport nm (Node (x, y) D _) = sum . map (\off -> scoreFnForCoords nm (x, y - off)) $ [0 .. 3]

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)