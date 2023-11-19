module Day15 where

import Data.Foldable (traverse_)
import Data.List (sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

data Coords = C {x :: Int, y :: Int} deriving (Show)

data InputLine = IL {beaconLocation :: Coords, sensorLocation :: Coords} deriving (Show)

data ExclusionZone = EZ {centre :: Coords, range :: Int} deriving (Show)

data Interval = Interval {start :: Int, end :: Int} deriving (Show)

-- I think I can do interval intersections in this 1d case...

part1 :: Int -> IO ()
part1 i = do
  input <- getLines "./fixtures/input15.txt"
  traverse_ print . getExclusionCountAtYCoord i . map (toExclusionZone . parseLine) $ input

part2 :: Int -> IO ()
part2 i = do
  input <- getLines "./fixtures/input15.txt"
  let parsed = map (toExclusionZone . parseLine) input
  let res = filter (\(_, xs) -> (length xs) > 1) . map (\y -> (y, getExclusionCountAtYCoord y parsed)) $ [0 .. i]
  traverse_ print res

getExclusionCountAtYCoord :: Int -> [ExclusionZone] -> [Interval]
getExclusionCountAtYCoord i = mergeIntervals . mapMaybe (getIntervalAtYcoord i)

mergeIntervals :: [Interval] -> [Interval]
mergeIntervals = mergeIntervals' . sortOn start
  where
    mergeIntervals' :: [Interval] -> [Interval]
    mergeIntervals' (Interval lo1 hi1 : Interval lo2 hi2 : rest)
      | hi1 >= lo2 - 1 =
          mergeIntervals' (Interval (min lo1 lo2) (max hi1 hi2) : rest)
    mergeIntervals' (interval : rest) = interval : mergeIntervals' rest
    mergeIntervals' [] = []

getIntervalAtYcoord :: Int -> ExclusionZone -> Maybe Interval
getIntervalAtYcoord i (EZ coords range) =
  let blah = abs (y coords - i)
   in if blah > range
        then Nothing
        else
          Just
            (Interval (x coords - (range - blah)) ((x coords + (range - blah))))

toExclusionZone :: InputLine -> ExclusionZone
toExclusionZone (IL b s) =
  let dist = abs (x b - x s) + abs (y b - y s) in EZ s dist

parseLine :: String -> InputLine
parseLine s =
  let [a, b] = splitOn ":" s
   in IL (parseCoords . drop 22 $ b) (parseCoords . drop 10 $ a)

parseCoords :: String -> Coords
parseCoords s =
  let [a, b] = splitOn "," s in C (read . drop 2 $ a) (read . drop 3 $ b)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)
