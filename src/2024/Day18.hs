{-# LANGUAGE TupleSections #-}

module Day18 where

import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec as P (char, digit, many1, newline, sepBy)
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.Dijkstra (DijkstraResult (FoundEndNode, QueueEmptied), dijkstra)

type Coord = (Int, Int)

data GridSize = GS {_width :: Int, _height :: Int}

type Grid = S.Set Coord

type Result = DijkstraResult Coord Int

part1 :: IO Result
part1 = do
  input <- getInput
  return . solve' (take fallenBits input) $ gridSize
  where
    gridSize = GS 71 71
    fallenBits = 1024

part2 :: IO Coord
part2 = do
  input <- getInput
  let searchFunction fallenBits = solve' (take fallenBits input) gridSize
  let initBounds = BSB 1025 3449 (searchFunction 1025) (searchFunction 3449)
  let res = binSearch initBounds searchFunction
  return (input !! (res - 1))
  where
    gridSize = GS 71 71

data BinSearchBounds = BSB {_lo :: Int, _hi :: Int, _loResult :: Result, _hiResult :: Result}

-- probably worth generalising and popping into a module if you use anything similar two more times
binSearch :: BinSearchBounds -> (Int -> Result) -> Int
binSearch (BSB lo hi loResult hiResult) f =
  if hi - lo == 1
    then hi
    else
      ( case (loResult, midResult, hiResult) of
          (FoundEndNode _, FoundEndNode _, QueueEmptied) -> binSearch (BSB mid hi midResult hiResult) f
          (FoundEndNode _, QueueEmptied, QueueEmptied) -> binSearch (BSB lo mid loResult midResult) f
          _ -> error "who ordered that???"
      )
  where
    mid = (hi + lo) `div` 2
    midResult = f mid

solve' :: [Coord] -> GridSize -> Result
solve' xs gs = solve gs grid
  where
    grid = mkGrid gs (S.fromList xs)

solve :: GridSize -> Grid -> Result
solve (GS width height) grid = dijkstra neighbourGetter isEndNode startNode
  where
    neighbourGetter :: Coord -> [(Coord, Int)]
    neighbourGetter = map (,1) . (neighbourMap M.!)

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
inputParser = lineParser `sepBy` newline
  where
    lineParser = (,) <$> intParser <* char ',' <*> intParser
    intParser = read <$> many1 digit
