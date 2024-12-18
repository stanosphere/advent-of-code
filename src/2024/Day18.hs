module Day18 where

import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.Dijkstra2 (Result (QueueEmptied), dijkstra)

type Coord = (Int, Int)

data GridSize = GS {_width :: Int, _height :: Int}

type Grid = S.Set Coord

part1 :: IO (Maybe (Result Coord Int))
part1 = do
  input <- getInput
  return . solve' (take fallenBits input) $ gridSize
  where
    gridSize = GS 71 71
    fallenBits = 1024

part2 :: IO Coord
part2 = do
  input <- getInput
  let res =
        fst
          . fromJust
          . find ((== Just QueueEmptied) . snd)
          . map (\fallenBits -> (fallenBits, solve' (take fallenBits input) gridSize))
          $ fallenBitsToTry
  return (input !! (res - 1))
  where
    gridSize = GS 71 71
    fallenBitsToTry = [1024 ..]

solve' :: [Coord] -> GridSize -> Maybe (Result Coord Int)
solve' xs gs = solve gs grid
  where
    grid = mkGrid gs (S.fromList xs)

solve :: GridSize -> Grid -> Maybe (Result Coord Int)
solve (GS width height) grid = dijkstra neighbourGetter isEndNode startNode
  where
    neighbourGetter :: Coord -> [(Coord, Int)]
    neighbourGetter = map (\x -> (x, 1)) . (neighbourMap M.!)

    isEndNode :: Coord -> Bool
    isEndNode coord = coord == (width - 1, height - 1)

    startNode :: Coord
    startNode = (0, 0)

    neighbourMap =
      M.fromList
        . map (\n -> (n, nodeToNeighbours grid n))
        . S.toList
        $ grid

nodeToNeighbours :: Grid -> Coord -> [Coord]
nodeToNeighbours grid (x, y) = filter (`S.member` grid) [(x, y - 1), (x, y + 1), (x + 1, y), (x - 1, y)]

-- this will make a grid where we remove all the stuff we don't need
-- ie where the falling bits are
-- suppose I could have just done a set difference actually...
mkGrid :: GridSize -> S.Set Coord -> Grid
mkGrid (GS width height) bitSpaces =
  S.fromList
    [ (x, y)
      | x <- [0 .. width - 1],
        y <- [0 .. height - 1],
        S.notMember (x, y) bitSpaces
    ]

getInput :: IO [Coord]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input18.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [Coord]
inputParser = lineParser `P.sepBy` P.newline
  where
    lineParser :: Parser Coord
    lineParser = do
      x <- intParser
      _ <- P.char ','
      y <- intParser
      return (x, y)
    intParser :: Parser Int
    intParser = read <$> P.many1 P.digit