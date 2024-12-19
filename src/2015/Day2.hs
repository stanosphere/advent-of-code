module Day2 where

import Text.Parsec as P (char, digit, many1, newline, sepBy)
import Text.ParserCombinators.Parsec (Parser, parse)

data Box = Box {_w :: Int, _h :: Int, _d :: Int} deriving (Show)

part1 :: IO Int
part1 =
  sum
    . map wrappingPaperNeeded
    . unsafeParse inputParser
    <$> readFile "./fixtures/input2.txt"

part2 :: IO Int
part2 =
  sum
    . map ribbonNeeded
    . unsafeParse inputParser
    <$> readFile "./fixtures/input2.txt"

ribbonNeeded :: Box -> Int
ribbonNeeded (Box w h d) = vol + smallestPerimeter
  where
    vol = product [w, h, d]
    smallestPerimeter = 2 * minimum [w + h, h + d, d + w]

wrappingPaperNeeded :: Box -> Int
wrappingPaperNeeded (Box w h d) = (2 * sum [side1, side2, side3]) + minimum [side1, side2, side3]
  where
    side1 = w * h
    side2 = h * d
    side3 = d * w

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [Box]
inputParser = lineParser `sepBy` newline
  where
    -- 22x29x19
    -- TODO this style of writing parsers is a bit nicer than the do notation init!
    lineParser = Box <$> intParser <* char 'x' <*> intParser <* char 'x' <*> intParser
    intParser = read <$> many1 digit
