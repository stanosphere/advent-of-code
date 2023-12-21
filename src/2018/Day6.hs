{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

-- I really should look up nice ways to do this on the reddit thread, or just think a bit harder!
module Day6 where

import Data.List.Split (splitOn)
import Data.Map qualified as M (Map, elems, filter)
import Utils.Grouping (groupBy')

data Point = P {x :: Int, y :: Int} deriving (Show)

newtype NodeId = NodeId {value :: Int} deriving (Show, Eq, Ord)

data Node = N {position :: Point, nodeId :: NodeId}

data GridSize = GS
  { minX :: Int,
    minY :: Int,
    maxX :: Int,
    maxY :: Int
  }
  deriving (Show)

type PointsByNodes = M.Map (Maybe NodeId) [(Point, Maybe NodeId)]

-- real input is 50 coords on an infnite grid
-- answer is 3969, runs in 3.89 secs
-- feel like I could have done this by sort of propegating out tendrils from each "node"
-- also I can tidy this up a bit now that I know what part 2 look like
part1 :: IO ()
part1 = do
  inp <- getLines "./fixtures/input6.txt"
  let nodePositions = map parsePoint inp
  let grid = getGridSize nodePositions
  let nodes = zipWith (\i p -> N p (NodeId i)) [0 ..] nodePositions
  let allPoints = getAllRelevantPoints grid
  print
    . maximum
    . map length
    . M.elems
    . removeInfiniteSlices grid
    . groupBy' snd
    . map (\p -> (p, findNearestNode nodes p))
    $ allPoints

isInInfiniteSlice :: GridSize -> Point -> Bool
isInInfiniteSlice (GS minX minY maxX maxY) (P x y) =
  x == minX || x == maxX || y == minY || y == maxY

removeInfiniteSlices :: GridSize -> PointsByNodes -> PointsByNodes
removeInfiniteSlices gs = M.filter (not . any (\(p, _) -> isInInfiniteSlice gs p))

getAllRelevantPoints :: GridSize -> [Point]
getAllRelevantPoints (GS minX minY maxX maxY) = [P x y | x <- [minX .. maxX], y <- [minY .. maxY]]

-- probably shouldn't hard code the offset but whatevs lol
getAllRelevantPointsForPart2 :: GridSize -> [Point]
getAllRelevantPointsForPart2 (GS minX minY maxX maxY) = [P x y | x <- [minX - 200 .. maxX + 200], y <- [minY - 200 .. maxY + 200]]

getGridSize :: [Point] -> GridSize
getGridSize =
  foldl
    (\(GS minX minY maxX maxY) (P x y) -> GS (min x minX) (min y minY) (max x maxX) (max y maxY))
    (GS 1000 1000 0 0)

parsePoint :: String -> Point
parsePoint s = let [x, y] = splitOn ", " s in P (read x) (read y)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

manhattanDist :: Point -> Point -> Int
manhattanDist (P x1 y1) (P x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- if it's a tie return nothing!
-- probably shouldn't hardcode 1000 lol
findNearestNode :: [Node] -> Point -> Maybe NodeId
findNearestNode nodes point =
  let (candidates, _) = foldl (folder point) ([], 1000) nodes
   in case candidates of
        [oneWinner] -> Just . nodeId $ oneWinner
        _ -> Nothing
  where
    folder :: Point -> State -> Node -> State
    folder p (candidates, smallestDistance) node = newState
      where
        manhattan = manhattanDist p (position node)
        newState
          | manhattan == smallestDistance = (node : candidates, manhattan)
          | manhattan < smallestDistance = ([node], manhattan)
          | otherwise = (candidates, smallestDistance)

type State = ([Node], Int)

-- so for part 2 I reckon since 10,000 / 50 = 200
-- I can use an upper bound where I go like a halo of 200 away from the extrema
-- which is like a bit annoying and definitely a massive overestimate
-- means grid will be about 700 * 700
-- and 700 * 700 * 50 is 24,500,000 so that's quite a few iterations but nothing toooo bad I don't think
-- also I'm going to assume the region is contiguous, IDK if it will be IRL!!!
-- there MUST be a way of determining if a set of lattice points are contiguous though
-- -- I guess there's always the classic connected components approach that will work on any graph
-- -- but I reckon there might be a nice way for a given metric (e.g. euclidean or manhattan)
-- answer is 42123, runs in 13.56 secs
part2 :: IO ()
part2 = do
  inp <- getLines "./fixtures/input6.txt"
  let nodePositions = map parsePoint inp
  let grid = getGridSize nodePositions
  let nodes = zipWith (\i p -> N p (NodeId i)) [0 ..] nodePositions
  print . length . filter (pointIsInRegion nodes) . getAllRelevantPointsForPart2 $ grid

-- probbaly shouldn't hardcode 10,000
pointIsInRegion :: [Node] -> Point -> Bool
pointIsInRegion nodes p =
  let sumOfDists = foldl (\s n -> s + manhattanDist p (position n)) 0 nodes
   in sumOfDists < 10000