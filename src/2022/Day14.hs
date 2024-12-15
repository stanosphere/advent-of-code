module Day14 where

import Data.Foldable (traverse_)
import Data.List (tails)
import Data.List.Split (splitOn)
import qualified Data.Map as M
  ( Map,
    filter,
    fromList,
    insert,
    keysSet,
    lookup,
    mapKeys,
    member,
    notMember,
  )
import qualified Data.Set as S
  ( Set,
    fromList,
    map,
    toList,
    unions,
  )

data Occupation = Sand | Rock deriving (Show, Eq)

type OccupationMap = M.Map Coords Occupation

data OccupationState = OC {mp :: OccupationMap, bounds :: Bounds}

data OccupationState' = OC' {mp' :: OccupationMap, blocked :: Bool}

data Bounds = InBounds | OutOfBounds deriving (Eq)

data Coords = C {x :: Int, y :: Int} deriving (Show, Ord, Eq)

part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input14.txt"
  showRocks
    . mp
    . last
    . takeWhile ((== InBounds) . bounds)
    . iterate (processSandGrain (C 500 0))
    $ (OC (parseInput input) InBounds)

part2 :: IO ()
part2 = do
  input <- getLines "./fixtures/input14.txt"
  showRocks
    . mp'
    . last
    . takeWhile (not . blocked)
    . iterate (processSandGrain' (C 500 0))
    $ (OC' (parseInput input) False)

processSandGrain' :: Coords -> OccupationState' -> OccupationState'
processSandGrain' (C x y) (OC' mp blocked)
  | M.member (C x 0) mp = OC' mp True
  | y == 163 + 1 = OC' (M.insert (C x y) Sand mp) blocked
  | M.notMember (C x (y + 1)) mp = processSandGrain' (C x (y + 1)) (OC' mp blocked)
  | M.notMember (C (x - 1) (y + 1)) mp = processSandGrain' (C (x - 1) (y + 1)) (OC' mp blocked)
  | M.notMember (C (x + 1) (y + 1)) mp = processSandGrain' (C (x + 1) (y + 1)) (OC' mp blocked)
  | otherwise = OC' (M.insert (C x y) Sand mp) blocked

processSandGrain :: Coords -> OccupationState -> OccupationState
processSandGrain (C x y) (OC mp bounds)
  | y > 168 = OC mp OutOfBounds
  | M.notMember (C x (y + 1)) mp = processSandGrain (C x (y + 1)) (OC mp bounds)
  | M.notMember (C (x - 1) (y + 1)) mp = processSandGrain (C (x - 1) (y + 1)) (OC mp bounds)
  | M.notMember (C (x + 1) (y + 1)) mp = processSandGrain (C (x + 1) (y + 1)) (OC mp bounds)
  | otherwise = OC (M.insert (C x y) Sand mp) bounds

-- helper to plot the output :)
showRocks :: OccupationMap -> IO ()
showRocks mp =
  let locs = M.keysSet mp
      xs = S.map x locs
      ys = S.map y locs
      maxX = maximum xs
      minX = minimum xs
      maxY = maximum ys
      minY = minimum ys
      mp' = M.mapKeys (\(C x y) -> C (x - minX) (y - minY)) mp
      grid =
        [ [toPrintable (C x y) mp' | x <- [0 .. (maxX - minX)]]
          | y <- [0 .. (maxY - minY)]
        ]
   in print maxY *> print (count (== Sand) mp) *> traverse_ print grid
  where
    toPrintable :: Coords -> OccupationMap -> Char
    toPrintable c mp = case M.lookup c mp of
      Nothing -> ' '
      Just Sand -> '.'
      Just Rock -> '#'

count :: (v -> Bool) -> M.Map k v -> Int
count p = length . M.filter p

-- just parsing stuff below here really
parseInput :: [String] -> M.Map Coords Occupation
parseInput =
  fromSet Rock
    . S.unions
    . map pathToPoints
    . map (map parseCoords)
    . map
      (splitOn " -> ")

pathToPoints :: [Coords] -> S.Set Coords
pathToPoints = S.fromList . concatMap getAllPointsInbetween . slidingPairs

parseCoords :: String -> Coords
parseCoords s = let [x, y] = splitOn "," s in C (read x) (read y)

getAllPointsInbetween :: (Coords, Coords) -> [Coords]
getAllPointsInbetween (C x1 y1, C x2 y2)
  | x1 == x2 && y1 < y2 = map (C x1) [y1 .. y2]
  | x1 == x2 && y1 >= y2 = map (C x1) [y2 .. y1]
  | y1 == y2 && x1 < x2 = map (`C` y1) [x1 .. x2]
  | y1 == y2 && x1 >= x2 = map (`C` y1) [x2 .. x1]
  | otherwise = undefined

slidingPairs :: [a] -> [(a, a)]
slidingPairs = map (\[x, y] -> (x, y)) . windows 2

windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((n ==) . length) . map (take n) . tails

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

fromSet :: (Ord k) => a -> S.Set k -> M.Map k a
fromSet x = M.fromList . map (\k -> (k, x)) . S.toList
