{-# LANGUAGE InstanceSigs #-}

module Day2 where

import Text.Parsec as P (digit, many, many1, newline, sepBy, tab)
import Text.ParserCombinators.Parsec (Parser, parse)

data Stats = Stats {minX :: Int, maxX :: Int}

instance Semigroup Stats where
  (<>) :: Stats -> Stats -> Stats
  (Stats min1 max1) <> (Stats min2 max2) = Stats (min min1 min2) (max max1 max2)

instance Monoid Stats where
  mempty :: Stats
  mempty = Stats maxBound minBound

part1 :: IO Int
part1 = getChecksum <$> getInput

part2 :: IO Int
part2 = sum . map findPairWithExactDivision <$> getInput

-- ok so for part 2 there is some pair of numbers where one divides the other exactly
-- and we want to find this pair and actually do the division
-- the input data is small enough that I think I can just do a carty prod (i.e. check all possible pairs)
-- we're told there's exactly one such pair so I can just use `head` recklessly
findPairWithExactDivision :: [Int] -> Int
findPairWithExactDivision xs =
  head
    [ x `div` y
      | x <- xs,
        y <- xs,
        x /= y,
        x `mod` y == 0
    ]

getChecksum :: [[Int]] -> Int
getChecksum = sum . map ((\(Stats lo hi) -> hi - lo) . getStats)

getStats :: [Int] -> Stats
getStats = foldMap (\x -> Stats x x)

getInput :: IO [[Int]]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input2.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [[Int]]
inputParser = lineParser `sepBy` newline
  where
    lineParser = intParser `sepBy` spacesParser
    intParser = read <$> many1 digit
    spacesParser = many tab
