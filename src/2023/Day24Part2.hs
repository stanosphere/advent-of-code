{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
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

import Data.Foldable
import Data.List (nub, sortOn, tails)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Ratio (Ratio, denominator, numerator, (%))
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))

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

part2 :: IO ()
part2 = do
  inp <- getLines "./fixtures/input24.txt"
  let hss = map (unsafeParse hailstoneParser) inp

  traverse_ print . sortOn snd . M.toList . M.filter (> 1) . frequencies . map (vx) $ hss
  print ""
  traverse_ print . sortOn snd . M.toList . M.filter (> 1) . frequencies . map (vy) $ hss
  print ""
  traverse_ print . sortOn snd . M.toList . M.filter (> 1) . frequencies . map (vz) $ hss
  print ""

frequencies :: Ord a => [a] -> M.Map a Int
frequencies = foldr (alter' (maybe 1 (+ 1))) M.empty

-- like alter but can't delete elements
alter' :: Ord k => (Maybe a -> a) -> k -> M.Map k a -> M.Map k a
alter' f = M.alter (Just . f)

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
