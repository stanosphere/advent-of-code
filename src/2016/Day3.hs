{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
module Day3 where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Text.Parsec as P (digit, many1, newline, parse, sepBy, spaces)
import Text.ParserCombinators.Parsec (Parser)

part1 :: IO Int
part1 = length . filter isTriangle <$> getInput

part2 :: IO Int
part2 =
  length
    . filter (\t -> isTriangle (t !! 0, t !! 1, t !! 2))
    . chunksOf 3
    . concat
    . transpose
    . map (\(x, y, z) -> [x, y, z])
    <$> getInput

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (x, y, z) = (x + y > z) && (y + z > x) && (z + x > y)

getInput :: IO [(Int, Int, Int)]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input3.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [(Int, Int, Int)]
inputParser = lineParser `sepBy` newline
  where
    lineParser = (,,) <$> intParser <*> intParser <*> intParser
    intParser = spaces *> (read <$> many1 digit)