module Day7 where

import Text.Parsec as P (char, choice, digit, letter, many1, newline, oneOf, sepBy, string, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)

data Instruction
  = INPUT Int String
  | AND String String String
  | OR String String String
  | LSHIFT String String Int
  | RSHIFT String String Int
  | NOT String String

instructionParser :: Parser Instruction
instructionParser = P.choice [inputParser, andParser, orParser, lshiftParser, rshiftParser, notParser]

-- 19138 -> b
inputParser :: Parser Instruction
inputParser = INPUT <$> intParser <* string " -> " <*> wireNameParser

-- hq AND hs -> ht
andParser :: Parser Instruction
andParser = AND <$> wireNameParser <* string " AND " <*> wireNameParser <* string " -> " <*> wireNameParser

-- kg OR kf -> kh
orParser :: Parser Instruction
orParser = OR <$> wireNameParser <* string " OR " <*> wireNameParser <* string " -> " <*> wireNameParser

lshiftParser :: Parser Instruction
lshiftParser = undefined

rshiftParser :: Parser Instruction
rshiftParser = undefined

notParser :: Parser Instruction
notParser = undefined

intParser :: Parser Int
intParser = read <$> many1 digit

wireNameParser :: Parser String
wireNameParser = many1 letter

-- 123 -> x
-- 456 -> y
-- x AND y -> d
-- x OR y -> e
-- x LSHIFT 2 -> f
-- y RSHIFT 2 -> g
-- NOT x -> h
-- NOT y -> i
