module Day5 where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)

data Point = Point
  { x :: Int,
    y :: Int
  }
  deriving (Show, Ord, Eq)

data LineSegment = Line
  { from :: Point,
    to :: Point
  }
  deriving (Show)

type LineCount = Int

type Grid = M.Map Point LineCount

-- 5690
part1 :: IO ()
part1 = do
  lines <- getLines "./fixtures/input5.txt"
  let lineSegments = map toLineSegment lines
  let horizontalAndVertical =
        filter (\ls -> isVertical ls || isHorizontal ls) lineSegments
  let points = foldl updateStateWithLineSegment M.empty horizontalAndVertical
  let res = M.size . M.filter (> 1) $ points
  print res

-- 17741
part2 :: IO ()
part2 = do
  lines <- getLines "./fixtures/input5.txt"
  let lineSegments = map toLineSegment lines
  let points = foldl updateStateWithLineSegment M.empty lineSegments
  let res = M.size . M.filter (> 1) $ points
  print res

-- actual computation here
updateStateWithLineSegment :: Grid -> LineSegment -> Grid
updateStateWithLineSegment g ls =
  foldl updateStateWithPoint g $ getLatticePoints ls

updateStateWithPoint :: Grid -> Point -> Grid
updateStateWithPoint g p = M.insertWith (+) p 1 g

-- line segment functions
isVertical :: LineSegment -> Bool
isVertical ls = (x . from $ ls) == (x . to $ ls)

isHorizontal :: LineSegment -> Bool
isHorizontal ls = (y . from $ ls) == (y . to $ ls)

getLatticePoints :: LineSegment -> [Point]
getLatticePoints ls
  | isVertical ls = getVerticalLatticePoints ls
  | isHorizontal ls = getHorizontalLatticePoints ls
  | otherwise = getSkewLatticePoints ls

getVerticalLatticePoints :: LineSegment -> [Point]
getVerticalLatticePoints ls =
  let [smallest, biggest] = sort [y . from $ ls, y . to $ ls]
   in map (Point (x . from $ ls)) [smallest .. biggest]

getHorizontalLatticePoints :: LineSegment -> [Point]
getHorizontalLatticePoints ls =
  let [smallest, biggest] = sort [x . from $ ls, x . to $ ls]
   in map (\x -> Point x (y . from $ ls)) [smallest .. biggest]

getSkewLatticePoints :: LineSegment -> [Point]
getSkewLatticePoints ls =
  let [minX, maxX] = sort [x . from $ ls, x . to $ ls]
      res = mapMaybe (mkNewPoint ls) [minX .. maxX]
   in res

mkNewPoint :: LineSegment -> Int -> Maybe Point
mkNewPoint ls xValue =
  let x1 = x . from $ ls
      y1 = y . from $ ls
      x2 = x . to $ ls
      y2 = y . to $ ls
      num = y2 * (xValue - x1) + y1 * (x2 - xValue)
      denom = x2 - x1
   in if num `rem` denom == 0
        then Just (Point xValue (num `div` denom))
        else Nothing

-- ugly parsing stuff below here
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toLineSegment :: String -> LineSegment
toLineSegment s =
  let x = splitOn "->" s
      from = head x
      to = x !! 1
      [x1, y1] = map (\p -> read p :: Int) . splitOn "," $ from
      [x2, y2] = map (\p -> read p :: Int) . splitOn "," $ to
   in Line (Point x1 y1) (Point x2 y2)
