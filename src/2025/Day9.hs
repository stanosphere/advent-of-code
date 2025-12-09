module Day9 where

import Text.Parsec as P (char, digit, many1, newline, sepBy)
import Text.ParserCombinators.Parsec (Parser, parse)

type Coords = (Int, Int)

-- oof part 2 is gonna be nasty
-- https://aoc.oppi.li/2.5-day-9.html#day-9
-- I took some inspiration from the above!

part2 :: IO Int
part2 = solvePart2 <$> getInput

solvePart2 :: [Coords] -> Int
solvePart2 loopCoords =
  maximum
    . map (uncurry rectArea)
    . filter (uncurry (rectWithinLoop allSidesOfLoop))
    . allPairs
    $ loopCoords
  where
    allSidesOfLoop = sides loopCoords
    sides (p : ps) = zip (p : ps) (ps ++ [p])
    sides [] = []

rectWithinLoop :: [(Coords, Coords)] -> Coords -> Coords -> Bool
rectWithinLoop allSidesOfLoop (x1, y1) (x2, y2) = not . rectOutsideLoop allSidesOfLoop (x1, y1) $ (x2, y2)

-- we check our rect against each side in the loop
-- if it ever intersects one of the sides we know we've crossed the loop!
-- so for each side we can make that check
-- a side is actually just a rectangle
-- and there's a relatively well known formula for computing rect intersections
-- it's essentially just 2 interval intersections: one for x dir, and one for y dir
rectOutsideLoop :: [(Coords, Coords)] -> Coords -> Coords -> Bool
rectOutsideLoop allSidesOfLoop (rx1, ry1) (rx2, ry2) = any sideIntersectsRect allSidesOfLoop
  where
    sideIntersectsRect ((sx1, sy1), (sx2, sy2)) =
      and
        [ max sx1 sx2 > min rx1 rx2,
          min sx1 sx2 < max rx1 rx2,
          max sy1 sy2 > min ry1 ry2,
          min sy1 sy2 < max ry1 ry2
        ]

part1 :: IO Int
part1 = maximum . map (uncurry rectArea) . allPairs <$> getInput

rectArea :: Coords -> Coords -> Int
rectArea (x1, y1) (x2, y2) = (1 + abs (x1 - x2)) * (1 + abs (y1 - y2))

allPairs :: [a] -> [(a, a)]
allPairs xs = [(c1, c2) | (i, c1) <- ys, (j, c2) <- ys, i < j]
  where
    ys = zip [0 :: Int ..] xs

getInput :: IO [Coords]
getInput = unsafeParse (lineParser `P.sepBy` P.newline) <$> readFile "./fixtures/input9.txt"
  where
    lineParser :: Parser Coords
    lineParser = (,) <$> intParser <* P.char ',' <*> intParser

    intParser :: Parser Int
    intParser = read <$> many1 digit

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res