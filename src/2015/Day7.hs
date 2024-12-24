module Day7 where

import Data.Bits (xor, (.&.), (.|.))
import Text.Parsec as P (char, choice, digit, letter, many1, newline, oneOf, sepBy, string, try, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)

data Instruction
  = INPUT Int String
  | AND String String String
  | OR String String String
  | LSHIFT String Int String
  | RSHIFT String Int String
  | NOT String String
  deriving (Show)

getInput :: IO [Instruction]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input7.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [Instruction]
inputParser = instructionParser `sepBy` newline

instructionParser :: Parser Instruction
instructionParser = P.choice . map try $ [inputParser, andParser, orParser, lshiftParser, rshiftParser, notParser]
  where
    -- 19138 -> b
    inputParser :: Parser Instruction
    inputParser =
      INPUT
        <$> (intParser <* string " -> ")
        <*> wireName

    -- hq AND hs -> ht
    andParser :: Parser Instruction
    andParser =
      AND
        <$> (wireName <* string " AND ")
        <*> (wireName <* string " -> ")
        <*> wireName

    -- kg OR kf -> kh
    orParser :: Parser Instruction
    orParser =
      OR
        <$> (wireName <* string " OR ")
        <*> (wireName <* string " -> ")
        <*> wireName

    lshiftParser :: Parser Instruction
    lshiftParser =
      LSHIFT
        <$> (wireName <* string " LSHIFT ")
        <*> (intParser <* string " -> ")
        <*> wireName

    rshiftParser :: Parser Instruction
    rshiftParser =
      RSHIFT
        <$> (wireName <* string " RSHIFT ")
        <*> (intParser <* string " -> ")
        <*> wireName

    notParser :: Parser Instruction
    notParser =
      NOT
        <$> (string "NOT " *> wireName)
        <*> (string " -> " *> wireName)

    intParser :: Parser Int
    intParser = read <$> many1 digit

    wireName :: Parser String
    wireName = many1 letter
