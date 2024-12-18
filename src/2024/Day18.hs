module Day18 where

import Data.Foldable (traverse_)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.Dijkstra (dijkstra)

type Coord = (Int, Int)

data GridSize = GS {_width :: Int, _height :: Int}

type Grid = S.Set Coord

part2 :: IO ()
part2 = do
  input <- getInput
  let gridSize = GS 71 71
  let fallenBitsToTry = [1024 ..]
  let res = map (\fallenBits -> (fallenBits, solve' (take fallenBits input) gridSize)) fallenBitsToTry
  traverse_ print res

part1 :: IO (Maybe (Coord, Int))
part1 = do
  input <- getInput
  let gridSize = GS 71 71
  let fallenBits = 1024
  let res = solve' (take fallenBits input) gridSize
  return res

solve' :: [Coord] -> GridSize -> Maybe (Coord, Int)
solve' xs gs = solve gs grid
  where
    grid = mkGrid gs (S.fromList xs)

solve :: GridSize -> Grid -> Maybe (Coord, Int)
solve (GS width height) grid = dijkstra scoreFn neighbourGetter isEndNode startNode
  where
    scoreFn :: Coord -> Coord -> Int
    scoreFn _ _ = 1

    neighbourGetter :: Coord -> [Coord]
    neighbourGetter n = neighbourMap M.! n

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