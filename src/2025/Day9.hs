module Day9 where

import Text.Parsec as P (char, digit, many1, newline, sepBy)
import Text.ParserCombinators.Parsec (Parser, parse)

type Coords = (Int, Int)

part1 :: IO Int
part1 = maximum . map (uncurry rectArea) . allPairs <$> getInput

rectArea :: Coords -> Coords -> Int
rectArea (x1, y1) (x2, y2) = (1 + abs (x1 - x2)) * (1 + abs (y1 - y2))

allPairs :: [a] -> [(a, a)]
allPairs xs = [(c1, c2) | (i, c1) <- ys, (j, c2) <- ys, i < j]
  where
    ys = zip [0 :: Int ..] xs

getInput :: IO [Coords]
getInput = unsafeParse (lineParser `P.sepBy` P.newline) <$> readFile "./fixtures/input9.txt"
  where
    lineParser :: Parser Coords
    lineParser = (,) <$> intParser <* P.char ',' <*> intParser

    intParser :: Parser Int
    intParser = read <$> many1 digit

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res