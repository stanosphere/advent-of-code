module Day12 where

import qualified Data.Map as M
import Text.Parsec as P (char, count, digit, many1, newline, oneOf, sepBy, string)
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.Grouping (groupMapReduce)

-- right so there are 3 categories of input
-- 1. definitely will not work: the total number of 1*1 tiles in the input exceeds the size of the grid
-- 2. definitely will work: all the of 3*3 tiles will fit in the grid
-- 2.a. this is because each of the shapes fits in a 3*3 grid
-- 3. might work - this is the nasty hard case where we actually have to solve the packing problem for real
-- 3.a. I have it on good authority that this case may never appear but I'll check for myself!

type Coord = (Int, Int)

type Shape = M.Map Coord Char

data GridConfig = GridConfig {_width :: Int, _height :: Int, _shapeCounts :: [Int]} deriving (Show)

data GridCategory = WillFit | WontFit | CantTell deriving (Show, Ord, Eq)

part1 :: IO (M.Map GridCategory Int)
part1 = do
  (shapes, gridConfigs) <- getInput
  return . groupMapReduce id (const 1) (+) . map (categoriseGrid shapes) $ gridConfigs

categoriseGrid :: [(Int, Shape)] -> GridConfig -> GridCategory
categoriseGrid shapes (GridConfig width height shapeCounts)
  | totalTiles > gridSize = WontFit
  | totalThreeByThreeTiles `div` 9 < ((width `div` 3) * (height `div` 3)) = WillFit
  | otherwise = CantTell
  where
    totalTiles = sum . zipWith (\s c -> c * shapeSize s) (map snd shapes) $ shapeCounts
    totalThreeByThreeTiles = sum shapeCounts
    gridSize = width * height

shapeSize :: Shape -> Int
shapeSize = length . filter (== '#') . M.elems

getInput :: IO ([(Int, Shape)], [GridConfig])
getInput = unsafeParse inputParser <$> readFile "./fixtures/input12.txt"
  where
    inputParser :: Parser ([(Int, Shape)], [GridConfig])
    inputParser = do
      shapes <- P.count 6 (shapeParser <* P.newline <* P.newline)
      gridConfigs <- gridLineParser `P.sepBy` P.newline
      return (shapes, gridConfigs)
      where
        -- e.g.
        -- 12x5: 1 0 1 0 2 2
        gridLineParser :: Parser GridConfig
        gridLineParser = do
          x <- intParser
          _ <- P.char 'x'
          y <- intParser
          _ <- P.string ": "
          ns <- intParser `P.sepBy` P.char ' '
          return (GridConfig x y ns)

        -- e.g.
        --
        -- 0:
        -- ###
        -- ##.
        -- ##.
        shapeParser :: Parser (Int, Shape)
        shapeParser = do
          i <- intParser
          _ <- P.char ':'
          _ <- P.newline
          -- for now just do the three lines thing
          xs <- P.many1 (P.oneOf "#.")
          _ <- P.newline
          ys <- P.many1 (P.oneOf "#.")
          _ <- P.newline
          zs <- P.many1 (P.oneOf "#.")
          return (i, toCoordinateMap [xs, ys, zs])

        intParser :: Parser Int
        intParser = read <$> P.many1 P.digit

        toCoordinateMap :: [String] -> Shape
        toCoordinateMap grid =
          M.fromList
            [ ((i, j), c)
              | (i, row) <- zipWithIndex grid,
                (j, c) <- zipWithIndex row
            ]

        zipWithIndex :: [a] -> [(Int, a)]
        zipWithIndex = zip [0 ..]

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res