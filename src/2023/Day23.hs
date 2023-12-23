module Day23 where

import Data.Foldable (find, traverse_)
import Data.Map qualified as M
import Data.Maybe (catMaybes)

type Coords = (Int, Int)

data Tile = Free | SlopeUp | SlopeDown | SlopeLeft | SlopeRight deriving (Eq, Show)

type Grid = M.Map Coords Tile

type Path = [Coords]

data State = State {unfinished :: [Path], finished :: [Path]} deriving (Show)

-- 2358
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

initState :: Coords -> [Coords]
initState coords = [coords]

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
  Just _ -> Just (coords)
  Nothing -> Nothing

getRightNode :: Grid -> Coords -> Maybe Coords
getRightNode nm coords = case M.lookup coords nm of
  Just SlopeLeft -> Nothing
  Just _ -> Just (coords)
  Nothing -> Nothing

getLeftNode :: Grid -> Coords -> Maybe Coords
getLeftNode nm coords = case M.lookup coords nm of
  Just SlopeRight -> Nothing
  Just _ -> Just (coords)
  Nothing -> Nothing

getUpNode :: Grid -> Coords -> Maybe Coords
getUpNode nm coords = case M.lookup coords nm of
  Just SlopeDown -> Nothing
  Just _ -> Just (coords)
  Nothing -> Nothing

getDownNode :: Grid -> Coords -> Maybe Coords
getDownNode nm coords = case M.lookup coords nm of
  Just SlopeUp -> Nothing
  Just _ -> Just (coords)
  Nothing -> Nothing

filterNeighbours :: Path -> [Coords] -> [Coords]
filterNeighbours p = filter (`notElem` p)

getNeighbours :: Grid -> Coords -> Tile -> [Coords]
getNeighbours g (x, y) Free = catMaybes [getLeftNode g (x - 1, y), getRightNode g (x + 1, y), getDownNode g (x, y + 1), getUpNode g (x, y - 1)]
getNeighbours g (x, y) SlopeLeft = catMaybes [getLeftNode g (x - 1, y)]
getNeighbours g (x, y) SlopeRight = catMaybes [getRightNode g (x + 1, y)]
getNeighbours g (x, y) SlopeUp = catMaybes [getUpNode g (x, y - 1)]
getNeighbours g (x, y) SlopeDown = catMaybes [getDownNode g (x, y + 1)]

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
