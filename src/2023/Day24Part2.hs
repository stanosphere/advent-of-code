{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day24Part2 where

-- I reckon the name of the game is simultaneous equations
-- Each line is just a vector equation like (x, y, z) = (x0 + vx * t, y0 + vy * t, z0 + vz * t)
-- I can then say that my line is (x, y, z) = (a + b * t, c + d * t, e + f* t)
-- I can set my line's equation equal to the other equations
-- each time I do I'll eliminate t to give me relationships between a,b,c,d,e,f
-- if I do this until I'e got six equations I should be able to solve
-- the reason this works is that the `t` in each parametric equation really is the same!
-- usually one would have to use a `t` for one line and a different `s` for the other

-- so let's test this out on the example...
-- a + b * t = 19 - 2 * t
-- c + d * t = 13 + t
-- e + f * t = 30 - 2 * t
-- this will give me three expressions for t which I can set equal to each other
-- t = (19 - a) / (2 + b)
-- t = (13 - c) / (d - 1)
-- t = (30 - e) / (2 + f)
-- which gives me three non linear simultaneous equations
-- (19 - a) * (d - 1) = (13 - c) * (2 + b)
-- (19 - a) * (2 + f) = (30 - e) * (2 + b)
-- (13 - c) * (2 + f) = (30 - e) * (d - 1)
-- which is honestly rather disgusting
-- we can obtain three further simultaneous equations with the second hailstone
-- I won't show the working, I'll just write down the answer
-- (18 - a) * (d + 1) = (19 - c) * (b + 1)
-- (18 - a) * (f + 2) = (22 - e) * (b + 1)
-- (19 - c) * (f + 2) = (22 - e) * (d + 1)
-- indeed the general form is
-- (x0 - a) * (d - vy) = (y0 - c) * (b - vx)
-- (x0 - a) * (f - vz) = (z0 - e) * (b - vx)
-- (y0 - c) * (f - vz) = (z0 - e) * (d - vy)
-- I think to approach solving this it might be simplest to obtain 4 equations linking a,b,c,d
-- Solve those simultaneously and then worry about the rest
-- I think that's the simplest I can make it for now
-- here are the 4 equations we would need
-- (19 - a) * (d - 1) = (13 - c) * (b + 2)
-- (18 - a) * (d + 1) = (19 - c) * (b + 1)
-- (20 - a) * (d + 2) = (25 - c) * (b + 2)
-- (12 - a) * (d + 2) = (31 - c) * (b + 1)
-- plugging this into wolfram alpha we obtain 2 solutions (makes sense these equations are quadratic after all)
-- one of them is indeed a = 24, b = -3, c = 13, d = 1
-- which is correct
-- the question now is how do I do what wolfram alpha did?
-- I suppose tactically I would start by using the duplicate b + 2 and b + 1 terms as well as the duplicate d + 2 terms to simplify stuff a bit
-- ok so pen and paper this is like really intractable
-- need something else...
-- so someone spotted a nice manipulation that you can do so I'm just gonna borrow that!
-- https://www.reddit.com/r/adventofcode/comments/18q40he/2023_day_24_part_2_a_straightforward_nonsolver/
-- I absolutely love this sort o thing!

import Data.Ratio (Ratio, denominator, numerator, (%))
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))
import Utils.Linear (solve)

data HailStone = HS
  { x0 :: Integer,
    y0 :: Integer,
    z0 :: Integer,
    vx :: Integer,
    vy :: Integer,
    vz :: Integer
  }
  deriving (Show)

toDouble :: Ratio Int -> Double
toDouble x = fromIntegral n / fromIntegral d
  where
    n = numerator x
    d = denominator x

part2 :: IO Rational
part2 = do
  inp <- getLines "./fixtures/input24.txt"
  let hss = map (unsafeParse hailstoneParser) inp

  let i = head hss
  let js = take 4 . tail $ hss
  let xyEquations = toRat . map (makeEquationXY i) $ js
  let xzEquations = toRat . map (makeEquationXZ i) $ js

  let [x, y, _, _] = solve xyEquations
  let [_, z, _, _] = solve xzEquations
  let res = x + y + z

  return res

toRat :: [[Integer]] -> [[Rational]]
toRat = map (map (% 1))

makeEquationXY :: HailStone -> HailStone -> [Integer]
makeEquationXY i j =
  [ vy i - vy j,
    vx j - vx i,
    y0 j - y0 i,
    x0 i - x0 j,
    (x0 i * vy i) - (y0 i * vx i) - (x0 j * vy j) + (y0 j * vx j)
  ]

makeEquationXZ :: HailStone -> HailStone -> [Integer]
makeEquationXZ i j =
  [ vz i - vz j,
    vx j - vx i,
    z0 j - z0 i,
    x0 i - x0 j,
    (x0 i * vz i) - (z0 i * vx i) - (x0 j * vz j) + (z0 j * vx j)
  ]

-- parsing stuff
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

-- could have use sepBy and such but whatevs
hailstoneParser :: Parser HailStone
hailstoneParser = do
  x0 <- intParser
  _ <- P.string ", "
  y0 <- intParser
  _ <- P.string ", "
  z0 <- intParser
  _ <- P.string " @ "
  vx <- intParser
  _ <- P.string ", "
  vy <- intParser
  _ <- P.string ", "
  vz <- intParser
  return (HS x0 y0 z0 vx vy vz)

-- well maybe I should be more strict about the structure and have the '-' HAVE TO appear at the the front
intParser :: Parser Integer
intParser = read <$> P.many1 (P.digit <|> P.char '-')

toyInput :: [String]
toyInput =
  [ "19, 13, 30 @ -2, 1, -2",
    "18, 19, 22 @ -1, -1, -2",
    "20, 25, 34 @ -2, -2, -4",
    "12, 31, 28 @ -1, -2, -1",
    "20, 19, 15 @ 1, -5, -3"
  ]
