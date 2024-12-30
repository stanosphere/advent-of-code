module Day8 where

import Text.Parsec as P (char, choice, digit, many1, newline, parse, sepBy, string, try)
import Text.ParserCombinators.Parsec (Parser)

data Command
  = Rect Int Int
  | RotateRow Int Int
  | RotateCol Int Int
  deriving (Show)

getInput :: IO [Command]
getInput = unsafeParse (lineParser `sepBy` newline) <$> readFile "./fixtures/input8.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

lineParser :: Parser Command
lineParser = choice . map try $ [rect, rotateRow, rotateCol]
  where
    rect = Rect <$> (string "rect " *> int) <*> (char 'x' *> int)
    rotateRow = RotateRow <$> (string "rotate row y=" *> int) <*> (string " by " *> int)
    rotateCol = RotateCol <$> (string "rotate column x=" *> int) <*> (string " by " *> int)

    int = read <$> many1 digit