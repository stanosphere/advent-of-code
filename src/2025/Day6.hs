module Day6 where

import Data.Containers.ListUtils (nubOrd)
import Data.List (transpose, uncons)
import Data.Maybe (mapMaybe)
import Text.Parsec as P (char, digit, many1, oneOf, option, sepBy, sepEndBy, space, spaces, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)

data Op = Multiply | Add deriving (Show)

data Column = Column {_op :: Op, _numbers :: [Int]} deriving (Show)

part1 :: IO Int
part1 = sum . map processColumn <$> getInput
  where
    processColumn :: Column -> Int
    processColumn (Column Multiply xs) = product xs
    processColumn (Column Add xs) = sum xs

getInput :: IO [Column]
getInput = mapMaybe (toColumn . reverse) . transpose . map (unsafeParse lineParser) . lines <$> readFile "./fixtures/input6.txt"
  where
    toColumn :: [String] -> Maybe Column
    toColumn blah = do
      (h, t) <- uncons blah
      op <- toOp h
      return (Column op (map read t))
      where
        toOp :: String -> Maybe Op
        toOp "*" = Just Multiply
        toOp "+" = Just Add
        toOp _ = Nothing

    lineParser :: Parser [String]
    lineParser = P.spaces *> (P.many1 (P.digit P.<|> P.oneOf "*+") `P.sepEndBy` P.spaces)

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res