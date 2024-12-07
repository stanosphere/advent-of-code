module Day10 where

import qualified Data.Set as S

type Coord = (Int, Int)

type AsteroidPositions = [Coord]

type Slope = (Int, Int)

-- for each point work out the slope to all other asteroids in lowest terms
-- one with the most distinct slopes wins

part1 :: IO Int
part1 = solve . toGrid <$> getInput

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