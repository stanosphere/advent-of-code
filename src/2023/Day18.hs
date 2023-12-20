module Day18 where

import Data.Foldable (traverse_)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse, (<|>))

-- I reckon Pick's theorem might just sort us out
-- https://en.wikipedia.org/wiki/Pick%27s_theorem
-- unless the tunnels intersect I guess?
-- I'll draw the tunnels and see if that's the case...

data Direction = U | D | L | R deriving (Show)

data Instruction = Ins {dir :: Direction, steps :: Int, colour :: String} deriving (Show)

part1 = do
  inp <- getLines "./fixtures/input18Toy.txt"
  let instructions = map parseInstruction inp
  traverse_ print instructions

-- parsing stuff
instructionParser :: Parser Instruction
instructionParser = do
  dir <- directionParser
  P.space
  steps <- intParser
  P.space
  colour <- colourParser
  return (Ins dir steps colour)
  where
    colourParser :: Parser String
    colourParser = P.char '(' *> hexParser <* P.char ')'

    intParser :: Parser Int
    intParser = read <$> P.many1 P.digit

    hexParser :: Parser String
    hexParser = P.char '#' *> P.many1 (P.oneOf "abcdef0123456789")

    directionParser :: Parser Direction
    directionParser = (U <$ P.char 'U') <|> (D <$ P.char 'D') <|> (L <$ P.char 'L') <|> (R <$ P.char 'R')

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

parseInstruction :: String -> Instruction
parseInstruction = unsafeParse instructionParser

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)