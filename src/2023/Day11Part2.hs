module Day11Part2 where

import Data.List (transpose)

data Tile = Galaxy | Space deriving (Eq)

type Grid = [[Tile]]

type Coords = (Int, Int)

-- there's gotta be a better way of passing my `stretchFactor` all the way through than what I have here...

-- 0.37 secs
-- 9947476
part1 :: IO Int
part1 = do
  inp <- getLines "./fixtures/input11.txt"
  let grid = map parseLine inp
  let emptyRows = getEmptyRowIndexes grid
  let emptyCols = getEmptyColIndexes grid
  return . sum . getAllDistances 2 emptyRows emptyCols . geGalaxyCoords . map parseLine $ inp

-- 0.37 secs
-- 519939907614
part2 :: IO Int
part2 = do
  inp <- getLines "./fixtures/input11.txt"
  let grid = map parseLine inp
  let emptyRows = getEmptyRowIndexes grid
  let emptyCols = getEmptyColIndexes grid
  return . sum . getAllDistances 1000000 emptyRows emptyCols . geGalaxyCoords . map parseLine $ inp

getAllDistances :: Int -> [Int] -> [Int] -> [Coords] -> [Int]
getAllDistances stretchFactor emptyRows emptyCols coordList = [getDistance stretchFactor emptyRows emptyCols g1 g2 | g1 <- coordList, g2 <- coordList, g1 < g2]

getItemsBetween :: Ord a => a -> a -> [a] -> [a]
getItemsBetween x1 x2 = takeWhile (< hi) . dropWhile (< lo)
  where
    hi = max x1 x2
    lo = min x1 x2

getDistance :: Int -> [Int] -> [Int] -> Coords -> Coords -> Int
getDistance stretchFactor emptyRows emptyCols (x1, y1) (x2, y2) = manhattan + emptyRowSpace + emptyColSpace
  where
    manhattan = getManhattanDist (x1, y1) (x2, y2)
    emptyRowSpace = getEmptySpace stretchFactor y1 y2 emptyRows
    emptyColSpace = getEmptySpace stretchFactor x1 x2 emptyCols

getManhattanDist :: Coords -> Coords -> Int
getManhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getEmptySpace :: Int -> Int -> Int -> [Int] -> Int
getEmptySpace stretchFactor i1 i2 is = (* (stretchFactor - 1)) . length $ getItemsBetween i1 i2 is

geGalaxyCoords :: Grid -> [Coords]
geGalaxyCoords inp = [(x, y) | (y, xs) <- zip [0 ..] inp, (x, t) <- zip [0 ..] xs, t == Galaxy]

getEmptyRowIndexes :: Grid -> [Int]
getEmptyRowIndexes = map fst . filter (all (Space ==) . snd) . zip [0 ..]

getEmptyColIndexes :: Grid -> [Int]
getEmptyColIndexes = map fst . filter (all (Space ==) . snd) . zip [0 ..] . transpose

-- parsing
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseLine :: String -> [Tile]
parseLine = map parseTile
  where
    parseTile '#' = Galaxy
    parseTile '.' = Space
    parseTile _ = undefined
