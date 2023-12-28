module Day23Part2 where

import Data.Foldable (traverse_)
import Data.Set qualified as S

type Coords = (Int, Int)

type Grid = S.Set Coords

data Path = Path {current :: Coords, rest :: S.Set Coords} deriving (Show)

data State = State {unfinished :: [Path], finished :: [Path]} deriving (Show)

-- curiously dijkstra works well for the to input in part 2 but not the real input
-- probably either not honouring the "no going back" condition or something wrong with my dijkstra

-- all right! What if I use dijkstra on the graph of junctions rather than the graph of points...
-- looks like real input only has 7 junctions
-- and the real input has 34
-- surely max distance is findable on such a small graph
-- just have to create it...
-- I guess for each junction i can just follow the paths in the relevant direction until I meet a new junction and count how long the paths are
-- Should end up with each edge appearing twice: will make for a good sense check actually
-- I would also need to include my start and end nodes in the graph of course
-- IDK how efficient/inefficient this process of creating the graph will be
-- but once I have it I guess I could just save it somewhere
-- and in fact a similar approach could be used for part 1, but some edges get filtered out
-- in terms of the "no backtracking" condition I think we're good since once a node is visited in dijkstra it won't appear again
-- indeed even if dijkstra doesn't work a graph of 34 nodes will be simpler to work with than a graph with 9416 nodes
-- and maybe my naive approach used in part 1 will just work...

part2 = do
  grid <- getGrid <$> getLines "./fixtures/input23.txt"
  let allPoints = S.toList grid
  let junctions = filter (isJunction grid) allPoints
  let blah = map (\j -> (j, length . getNeighbours grid $ j)) junctions
  print blah
  print . S.size $ grid

-- could use intersections and diffs I suppose
getNeighbours :: Grid -> Coords -> [Coords]
getNeighbours g (x, y) = filter (`S.member` g) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

getGrid :: [String] -> Grid
getGrid inp = S.fromList [(x, y) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs, c /= '#']

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

isJunction :: Grid -> Coords -> Bool
isJunction g = (>= 3) . length . getNeighbours g

prettyPrintSymbolMap :: Int -> [Coords] -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[if (x, y) `elem` mp then '0' else ' ' | x <- counter] | y <- counter]
