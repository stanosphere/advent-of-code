module Day23 where

import Data.Foldable (find, traverse_)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

type Coords = (Int, Int)

data Tile = Free | SlopeUp | SlopeDown | SlopeLeft | SlopeRight deriving (Eq, Show)

type Grid = M.Map Coords Tile

type Path = [Coords]

data State = State {unfinished :: [Path], finished :: [Path]} deriving (Show)

-- curiously dijkstra works well for the to input in part 2 but not the real input
-- probably either not honouring the "no going back" condition or something wrong with my dijkstra

-- 2358
-- 3.07 secs
part1 :: IO (Maybe Int)
part1 = do
  grid <- getGrid <$> getLines "./fixtures/input23.txt"
  return . solve $ grid

solve :: Grid -> Maybe Int
solve grid =
  fmap (maximum . map ((\x -> x - 1) . length) . finished) . find (null . unfinished) . iterate (step grid realEnd) $ initState
  where
    toyEnd = (21, 22)
    realEnd = (139, 140)
    initState = State [[(1, 0)]] []

step :: Grid -> Coords -> State -> State
step g endCoords (State unfinished finished) =
  let stepped = concatMap (stepPath g) unfinished
      newFinished = filter (\p -> head p == endCoords) stepped
      newUnfinished = filter (\p -> head p /= endCoords) stepped
   in State newUnfinished (finished ++ newFinished)

stepPath :: Grid -> Path -> [Path]
stepPath g p = map (: p) . filterNeighbours p . getNeighbours g coords $ tile
  where
    coords = head p
    tile = g M.! coords

getNode :: Grid -> Coords -> Maybe Coords
getNode nm coords = case M.lookup coords nm of
  Just _ -> Just coords
  Nothing -> Nothing

getRightNode :: Grid -> Coords -> Maybe Coords
getRightNode nm (x, y) = case M.lookup (x + 1, y) nm of
  Just SlopeLeft -> Nothing
  Just _ -> Just (x + 1, y)
  Nothing -> Nothing

getLeftNode :: Grid -> Coords -> Maybe Coords
getLeftNode nm (x, y) = case M.lookup (x - 1, y) nm of
  Just SlopeRight -> Nothing
  Just _ -> Just (x - 1, y)
  Nothing -> Nothing

getUpNode :: Grid -> Coords -> Maybe Coords
getUpNode nm (x, y) = case M.lookup (x, y - 1) nm of
  Just SlopeDown -> Nothing
  Just _ -> Just (x, y - 1)
  Nothing -> Nothing

getDownNode :: Grid -> Coords -> Maybe Coords
getDownNode nm (x, y) = case M.lookup (x, y + 1) nm of
  Just SlopeUp -> Nothing
  Just _ -> Just (x, y + 1)
  Nothing -> Nothing

filterNeighbours :: Path -> [Coords] -> [Coords]
filterNeighbours p = filter (`notElem` p)

getNeighbours :: Grid -> Coords -> Tile -> [Coords]
getNeighbours g coords Free = mapMaybe ($ coords) [getLeftNode g, getRightNode g, getDownNode g, getUpNode g]
getNeighbours g coords SlopeLeft = mapMaybe ($ coords) [getLeftNode g]
getNeighbours g coords SlopeRight = mapMaybe ($ coords) [getRightNode g]
getNeighbours g coords SlopeUp = mapMaybe ($ coords) [getUpNode g]
getNeighbours g coords SlopeDown = mapMaybe ($ coords) [getDownNode g]

getGrid :: [String] -> Grid
getGrid inp = M.fromList [((x, y), toTile c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs, c /= '#']

toTile :: Char -> Tile
toTile '<' = SlopeLeft
toTile '>' = SlopeRight
toTile '^' = SlopeUp
toTile 'v' = SlopeDown
toTile '.' = Free
toTile _ = undefined

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

prettyPrintSymbolMap :: Int -> [Coords] -> IO ()
prettyPrintSymbolMap size mp =
  let counter = [0 .. size]
   in traverse_ putStrLn [[if (x, y) `elem` mp then '0' else ' ' | x <- counter] | y <- counter]
