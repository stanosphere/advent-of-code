{-# HLINT ignore "Use <$>" #-}
module Day24 where

import Data.List (tails)
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

data Bounds = Bounds
  { minX :: Ratio Integer,
    minY :: Ratio Integer,
    maxX :: Ratio Integer,
    maxY :: Ratio Integer
  }
  deriving (Show)

data Intersection = Intersection
  { x :: Ratio Integer,
    y :: Ratio Integer,
    t1 :: Ratio Integer, -- should always be integral really
    t2 :: Ratio Integer -- should always be integral really
  }
  deriving (Show)

toDouble :: Ratio Int -> Double
toDouble x = fromIntegral n / fromIntegral d
  where
    n = numerator x
    d = denominator x

part1 :: IO ()
part1 = do
  inp <- getLines "./fixtures/input24.txt"
  let hss = map (unsafeParse hailstoneParser) inp

  let minX = 200000000000000 % 1
  let maxX = 400000000000000 % 1
  let minY = 200000000000000 % 1
  let maxY = 400000000000000 % 1

  let bounds = Bounds minX minY maxX maxY

  let res = getIntersections bounds hss

  print . length $ res

getIntersections :: Bounds -> [HailStone] -> [Intersection]
getIntersections b =
  filter isInFuture
    . filter isInBounds
    . mapMaybe (uncurry getIntersection)
    . pairs
  where
    isInBounds (Intersection x y _ _) = x <= maxX b && x >= minX b && y <= maxY b && y >= minY b
    isInFuture (Intersection _ _ t1 t2) = t1 > 0 && t2 > 0

-- so I think it also has to be in the future for both particles...
getIntersection :: HailStone -> HailStone -> Maybe Intersection
getIntersection (HS x1 y1 _ vx1 vy1 _) (HS x2 y2 _ vx2 vy2 _) =
  if vx1 * vy2 == vx2 * vy1 then Nothing else Just (Intersection x y t1 t2)
  where
    x = (vx1 * vx2 * y2 - vx1 * vx2 * y1 + vx2 * vy1 * x1 - vx1 * vy2 * x2) % (vy1 * vx2 - vx1 * vy2)
    y = y1 % 1 + (vy1 % vx1) * (x - x1 % 1)
    t1 = (x - x1 % 1) / (vx1 % 1)
    t2 = (x - x2 % 1) / (vx2 % 1)

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

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