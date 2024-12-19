module Day3Part1 where

import Data.Functor (($>))
import Data.Maybe (catMaybes)
import Text.Parsec as P (anyToken, char, digit, many, many1, string, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse, try)

type Mult = (Int, Int)

part1 :: IO Int
part1 = processInput <$> getInput

processInput :: String -> Int
processInput = sum . map (uncurry (*)) . catMaybes . unsafeParse inputParser

inputParser :: Parser [Maybe Mult]
inputParser = many (Just <$> try multParser <|> (anyToken $> Nothing))
  where
    multParser = (,) <$> (string "mul(" *> intParser) <* char ',' <*> (intParser <* char ')')
    intParser = read <$> many1 digit

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

getInput :: IO String
getInput = readFile "./fixtures/input3.txt"