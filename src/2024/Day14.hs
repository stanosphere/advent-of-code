module Day14 where

import Data.Foldable (traverse_)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.Grouping (windows)

type Coord = (Int, Int)

data Robot = Robot {_position :: Coord, _velocity :: Coord} deriving (Show)

data GridSize = GS {_width :: Int, _height :: Int}

part1 :: IO Int
part1 = solve 100 (GS 101 103) <$> getInput

part2 :: IO ()
part2 = do
  robots <- getInput
  let gridSize = GS 101 103
  traverse_ (f . (\steps -> (steps, toDisplay gridSize . map (getRobotFinalPosition steps gridSize) $ robots))) [1 .. 10000]

f :: (Int, Maybe [String]) -> IO ()
f (x, y) = case y of
  Nothing -> print (show x ++ " (not making file though)")
  Just fileLines -> print x *> writeFile ("./out/file" ++ show x ++ ".txt") (unlines fileLines)

toDisplay :: GridSize -> [Coord] -> Maybe [String]
toDisplay (GS width height) robots = if shouldDisplay then Just display else Nothing
  where
    display = [[if (x, y) `elem` robots then '#' else ' ' | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]
    -- idea is that xmas tree probably has some robots in a nice line
    shouldDisplay = any (\line -> "#####" `elem` windows 5 line) display

solve :: Int -> GridSize -> [Robot] -> Int
solve steps (GS width height) robots = product [q1, q2, q3, q4]
  where
    finalPositions = map (getRobotFinalPosition steps (GS width height)) robots
    q1 = length . filter (\(x, y) -> x < (width `div` 2) && y < (height `div` 2)) $ finalPositions
    q2 = length . filter (\(x, y) -> x < (width `div` 2) && y > (height `div` 2)) $ finalPositions
    q3 = length . filter (\(x, y) -> x > (width `div` 2) && y < (height `div` 2)) $ finalPositions
    q4 = length . filter (\(x, y) -> x > (width `div` 2) && y > (height `div` 2)) $ finalPositions

getRobotFinalPosition :: Int -> GridSize -> Robot -> Coord
getRobotFinalPosition steps (GS width height) (Robot (x, y) (vx, vy)) =
  ((x + vx * steps) `mod` width, (y + vy * steps) `mod` height)

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