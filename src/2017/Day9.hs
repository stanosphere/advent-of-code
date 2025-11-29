module Day9 where

import Text.Parsec as P (anyChar, char, many, noneOf, sepBy, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)

-- there might be a more elegant way to model it but the below will have to do for now

data GroupItem = SubGroup Group | Garbage String deriving (Show)

type Group = [GroupItem]

part1 :: IO Int
part1 = scoreGroup 1 . unsafeParse groupParser <$> getInput

getInput :: IO String
getInput = readFile "./fixtures/input9.txt"

scoreGroup :: Int -> Group -> Int
scoreGroup currentScore = (currentScore +) . sum . map scoreSubGroup
  where
    scoreSubGroup :: GroupItem -> Int
    scoreSubGroup (Garbage _) = 0
    scoreSubGroup (SubGroup sg) = scoreGroup (currentScore + 1) sg

groupParser :: Parser Group
groupParser = P.char '{' *> groupItemParser `sepBy` P.char ',' <* P.char '}'
  where
    groupItemParser :: Parser GroupItem
    groupItemParser = subGroupParser P.<|> garbageParser

    subGroupParser :: Parser GroupItem
    subGroupParser = SubGroup <$> groupParser

    -- garbage starts with a `<` and ends with a `>`
    -- the '!' char is effectively an escape char
    garbageParser :: Parser GroupItem
    garbageParser = Garbage <$> (P.char '<' *> P.many (normalChar P.<|> escapedChar) <* P.char '>')
      where
        normalChar = P.noneOf "!>"
        escapedChar = P.char '!' *> P.anyChar

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res
