module Day23Part2 where

import Data.Foldable (find, traverse_)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

type Coords = (Int, Int)

type Grid = S.Set Coords

data Path = Path {current :: Coords, rest :: S.Set Coords} deriving (Show)

data State = State {unfinished :: [Path], finished :: [Path]} deriving (Show)

-- curiously dijkstra works well for the to input in part 2 but not the real input
-- probably either not honouring the "no going back" condition or something wrong with my dijkstra

part2 :: IO (Maybe Int)
part2 = do
  grid <- getGrid <$> getLines "./fixtures/input23.txt"
  return . solve $ grid

-- solve :: Grid -> Maybe Int
solve :: Grid -> Maybe Int
solve grid =
  fmap (maximum . map (S.size . rest) . finished) . find (null . unfinished) . iterate (step grid realEnd) $ initState
  where
    toyEnd = (21, 22)
    realEnd = (139, 140)
    initState = State [Path (1, 0) S.empty] []

step :: Grid -> Coords -> State -> State
step g endCoords (State unfinished finished) =
  let stepped = concatMap (stepPath g) unfinished
      newFinished = filter (\p -> current p == endCoords) stepped
      newUnfinished = filter (\p -> current p /= endCoords) stepped
   in State newUnfinished (finished ++ newFinished)

-- probably this and getNeighbours should be curried in same way
stepPath :: Grid -> Path -> [Path]
stepPath g p = map (appendPath p) . getNeighbours p $ g

appendPath :: Path -> Coords -> Path
appendPath (Path prev rest) c = Path c (S.insert prev rest)

-- could use intersections and diffs I suppose
getNeighbours :: Path -> Grid -> [Coords]
getNeighbours (Path (x, y) rest) g =
  filter (`S.member` g) . filter (`S.notMember` rest) $ [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

getGrid :: [String] -> Grid
getGrid inp = S.fromList [(x, y) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs, c /= '#']

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

prettyPrintSymbolMap :: Int -> [Coords] -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[if (x, y) `elem` mp then '0' else ' ' | x <- counter] | y <- counter]
