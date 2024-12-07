module Day10 where

import Data.Bifunctor (first, second)
import Data.List (transpose)
import Data.List.Extra (maximumOn, sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Utils.Grouping (groupBy')

type Coord = (Int, Int)

type AsteroidPositions = [Coord]

type Slope = (Int, Int)

-- for each point work out the slope to all other asteroids in lowest terms
-- one with the most distinct slopes wins

part1 :: IO Int
part1 = solve . toGrid <$> getInput

part2 :: IO Int
part2 = do
  input <- toGrid <$> getInput
  let stationPosition = findPositionForStation input
  let (x, y) = organiseByVaporisationOrder stationPosition input !! (200 - 1)
  return (x * 100 + y)

organiseByVaporisationOrder :: Coord -> AsteroidPositions -> [Coord]
organiseByVaporisationOrder stationPosition = concat . transpose . map snd . organiseByLineOfSight stationPosition

organiseByLineOfSight :: Coord -> AsteroidPositions -> [(Slope, [Coord])]
organiseByLineOfSight stationPosition =
  sortOn (first getAngle)
    . map (second (organiseByDistance stationPosition))
    . M.toList
    . groupBy' (getSlopeInLowestTerms stationPosition)
    . filter (/= stationPosition)

-- remember the y axis goes DOWN in computer science, not up like in normal science
-- so there'll be some adding of pis shenanigans here for sure
getAngle :: Slope -> Double
getAngle (dx, dy) = if res >= 0 then res else 2 * pi + res
  where
    res = atan2 (-fromIntegral dx) (fromIntegral dy)

organiseByDistance :: Coord -> [Coord] -> [Coord]
organiseByDistance (x0, y0) = sortOn (\(x, y) -> (x - x0) * (x - x0) + (y - y0) * (y - y0))

findPositionForStation :: AsteroidPositions -> Coord
findPositionForStation xs = maximumOn (`getAsteroidsSeen` xs) xs

solve :: AsteroidPositions -> Int
solve xs = maximum . map (`getAsteroidsSeen` xs) $ xs

getAsteroidsSeen :: Coord -> AsteroidPositions -> Int
getAsteroidsSeen x = S.size . S.fromList . map (getSlopeInLowestTerms x) . filter (/= x)

getSlopeInLowestTerms :: Coord -> Coord -> Slope
getSlopeInLowestTerms (x0, y0) (x1, y1) = (dx `div` gcd', dy `div` gcd')
  where
    dx = x0 - x1
    dy = y0 - y1
    gcd' = gcd (abs dx) (abs dy)

toGrid :: [String] -> AsteroidPositions
toGrid grid =
  [ (i, j)
    | (j, row) <- zipWithIndex grid,
      (i, char) <- zipWithIndex row,
      char == '#'
  ]
  where
    -- based on the function of the same name in scala
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input10.txt"