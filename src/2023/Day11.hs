module Day11 where

import Data.List (transpose)

data Tile = Galaxy | Space deriving (Eq)

type Grid = [[Tile]]

type Coords = (Int, Int)

-- 0.11 secs
-- 9947476
part1 :: IO Int
part1 = do
  inp <- getLines "./fixtures/input11.txt"
  return . sum . getAllDistances . geGalaxyCoords . applyExpansion . map parseLine $ inp

getAllDistances :: [Coords] -> [Int]
getAllDistances coordList = [getDistance g1 g2 | g1 <- coordList, g2 <- coordList, g1 < g2]

-- manhattan
getDistance :: Coords -> Coords -> Int
getDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

geGalaxyCoords :: Grid -> [Coords]
geGalaxyCoords inp = [(x, y) | (y, xs) <- zip [0 ..] inp, (x, t) <- zip [0 ..] xs, t == Galaxy]

applyExpansion :: Grid -> Grid
applyExpansion = expandRows . expandColumns
  where
    expandRows :: Grid -> Grid
    expandRows = concatMap (\row -> if all (Space ==) row then replicate 2 row else [row])
    expandColumns :: Grid -> Grid
    expandColumns = transpose . expandRows . transpose

-- parsing
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseLine :: String -> [Tile]
parseLine = map parseTile
  where
    parseTile '#' = Galaxy
    parseTile '.' = Space
    parseTile _ = undefined
