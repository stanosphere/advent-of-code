module Day24 where

import Data.Char (digitToInt)
import Text.Parsec as P
  ( choice,
    digit,
    letter,
    many1,
    newline,
    parse,
    sepBy,
    string,
    try,
    (<|>),
  )
import Text.ParserCombinators.Parsec (Parser)

data Gate
  = OR String String String
  | AND String String String
  | XOR String String String
  deriving (Show)

data Wire = Wire String Int deriving (Show)

getInput :: IO ([Wire], [Gate])
getInput = unsafeParse inputParser <$> readFile "./fixtures/input24.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser ([Wire], [Gate])
inputParser = do
  wires <- many1 (wireParser <* newline)
  _ <- newline
  gates <- gateParser `sepBy` newline
  return (wires, gates)
  where
    wireParser :: Parser Wire
    wireParser =
      Wire <$> (wireName <* string ": ") <*> (digitToInt <$> digit)

    gateParser :: Parser Gate
    gateParser = choice . map try $ [orGateParser, xorGateParser, andGateParser]
      where
        orGateParser :: Parser Gate
        orGateParser =
          OR <$> (wireName <* string " OR ") <*> (wireName <* string " -> ") <*> wireName

        xorGateParser :: Parser Gate
        xorGateParser =
          XOR <$> (wireName <* string " XOR ") <*> (wireName <* string " -> ") <*> wireName

        andGateParser :: Parser Gate
        andGateParser =
          AND <$> (wireName <* string " AND ") <*> (wireName <* string " -> ") <*> wireName

    wireName :: Parser String
    wireName = many1 (letter <|> digit)