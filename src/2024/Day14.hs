module Day14 where

import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)

type Coord = (Int, Int)

data Robot = Robot {_position :: Coord, _velocity :: Coord} deriving (Show)

data GridSize = GS {_width :: Int, _height :: Int}

part1 :: IO Int
part1 = solve (GS 101 103) <$> getInput

solve :: GridSize -> [Robot] -> Int
solve (GS width height) robots = product [q1, q2, q3, q4]
  where
    finalPositions = map (getRobotFinalPosition (GS width height)) robots
    q1 = length . filter (\(x, y) -> x < (width `div` 2) && y < (height `div` 2)) $ finalPositions
    q2 = length . filter (\(x, y) -> x < (width `div` 2) && y > (height `div` 2)) $ finalPositions
    q3 = length . filter (\(x, y) -> x > (width `div` 2) && y < (height `div` 2)) $ finalPositions
    q4 = length . filter (\(x, y) -> x > (width `div` 2) && y > (height `div` 2)) $ finalPositions

getRobotFinalPosition :: GridSize -> Robot -> Coord
getRobotFinalPosition (GS width height) (Robot (x, y) (vx, vy)) = ((x + vx * 100) `mod` width, (y + vy * 100) `mod` height)

getInput :: IO [Robot]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input14.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [Robot]
inputParser = P.sepBy robotParser P.newline
  where
    -- p=0,4 v=3,-3
    robotParser :: Parser Robot
    robotParser = do
      _ <- P.string "p="
      x <- intParser
      _ <- P.char ','
      y <- intParser
      _ <- P.string " v="
      vx <- intParser
      _ <- P.char ','
      vy <- intParser
      return (Robot (x, y) (vx, vy))

    intParser :: Parser Int
    intParser = read <$> P.many1 (P.digit P.<|> P.char '-')