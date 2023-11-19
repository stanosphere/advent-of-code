module Day9 where

import Data.Char (digitToInt)
import Data.Foldable
  ( find,
  )
import Data.Function (on)
import Data.List
  ( groupBy,
    sortOn,
  )
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S

type Coords = (Int, Int)

type Height = Int

type Risk = Int

type Grid = M.Map Coords Height

type Basins = S.Set Coords

part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input9.txt"
  let grid = toCoordinateMap input
  print . sum . map (\(_, height) -> getRisk height) . findAllMinima $ grid

part2 :: IO ()
part2 = do
  input <- getLines "./fixtures/input9.txt"
  let grid = toCoordinateMap input
  let basins = getBasins grid
  let locations = M.toList grid
  let locationsWithBasins =
        map (\x -> (fst x, snd x, getAssociatedBasin grid basins x)) locations
  let blah =
        take 4
          . sortOn ((* (-1)) . snd)
          . M.toList
          . M.map length
          . scalaGroupBy (\(_, _, basin) -> basin)
          $ locationsWithBasins
  print blah

toCoordinateMap :: [String] -> Grid
toCoordinateMap grid =
  M.fromList
    [ ((i, j), digitToInt elem)
      | (i, row) <- zipWithIndex grid,
        (j, elem) <- zipWithIndex row
    ]

getRisk :: Height -> Risk
getRisk = (+ 1)

findAllMinima :: Grid -> [(Coords, Height)]
findAllMinima grid = filter (isMinimum grid) (M.toList grid)

getAdjacentSquares :: Grid -> Coords -> [Height]
getAdjacentSquares grid (x, y) =
  mapMaybe (`M.lookup` grid) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getAdjacentSquares' :: Grid -> Coords -> [(Coords, Height)]
getAdjacentSquares' grid (x, y) =
  mapMaybe (`lookup'` grid) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

lookup' :: Ord k => k -> M.Map k v -> Maybe (k, v)
lookup' k m = fmap (\v -> (k, v)) (M.lookup k m)

isMinimum :: Grid -> (Coords, Height) -> Bool
isMinimum grid (coords, height) =
  all (> height) (getAdjacentSquares grid coords)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]

getBasins :: Grid -> Basins
getBasins = S.fromList . map fst . findAllMinima

getAssociatedBasin :: Grid -> Basins -> (Coords, Height) -> Maybe Coords
getAssociatedBasin grid basins (coords, height)
  | height == 9 =
      Nothing
  | coords `elem` basins =
      Just coords
  | otherwise =
      getNextCoordsToLookAt grid (coords, height)
        >>= getAssociatedBasin grid basins

getNextCoordsToLookAt :: Grid -> (Coords, Height) -> Maybe (Coords, Height)
getNextCoordsToLookAt grid (coords, height) =
  find (\(_, adjH) -> adjH < height) (getAdjacentSquares' grid coords)

scalaGroupBy :: Ord b => (a -> b) -> [a] -> M.Map b [a]
scalaGroupBy f =
  M.fromAscList
    . map (\x -> (f . head $ x, x))
    . groupBy ((==) `on` f)
    . sortOn f

-- ugly parsing stuff below here
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toyInput :: [String]
toyInput =
  ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"]
