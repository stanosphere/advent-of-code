module Day11 where

import Data.Foldable
import Data.List

data Tile = Galaxy | Space deriving (Eq)

instance Show Tile where
  show :: Tile -> String
  show Galaxy = "#"
  show Space = "."

type Coords = (Int, Int)

type GalaxyCoords = [Coords]

part1 :: IO Int
part1 = do
  inp <- getLines "./fixtures/input11.txt"
  return . sum . getAllDistances . geGalaxyCoords . applyExpansion . map parseLine $ inp

getAllDistances :: [Coords] -> [Int]
getAllDistances coordList = [getDistance g1 g2 | g1 <- coordList, g2 <- coordList, g1 < g2]

-- manhattan
getDistance :: Coords -> Coords -> Int
getDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

geGalaxyCoords :: [[Tile]] -> [Coords]
geGalaxyCoords inp = [(x, y) | (y, xs) <- zip [0 ..] inp, (x, t) <- zip [0 ..] xs, t == Galaxy]

applyExpansion :: [[Tile]] -> [[Tile]]
applyExpansion = expandRows . expandColumns
  where
    expandRows :: [[Tile]] -> [[Tile]]
    expandRows = concatMap (\row -> if all (Space ==) row then replicate 2 row else [row])
    expandColumns :: [[Tile]] -> [[Tile]]
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

printGrid :: [[Tile]] -> IO ()
printGrid = traverse_ (putStrLn . concatMap show)
