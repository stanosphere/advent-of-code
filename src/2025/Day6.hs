module Day6 where

import Data.Char (isDigit)
import Data.List (transpose, uncons)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Parsec as P (digit, many1, oneOf, sepEndBy, spaces, (<|>))
import Text.ParserCombinators.Parsec (Parser, parse)

data Op = Multiply | Add deriving (Show)

data Column = Column {_op :: Op, _numbers :: [Int]} deriving (Show)

part1 :: IO Int
part1 = sum . map processColumn <$> getInput

part2 :: IO Int
part2 = sum . map processColumn <$> getInput'

getInput' :: IO [Column]
getInput' = do
  transposed <- transpose . lines <$> readFile "./fixtures/input6.txt"
  let groupSize = length . head $ transposed
  return . mapMaybe processRow . splitOn [replicate groupSize ' '] $ transposed
  where
    -- ["1  *","24  ","356 ","    "] -> Column Multiply [1, 24, 356, 1]
    processRow row = do
      op <- toOp . last . head $ row
      return . Column op $ map (processElem op . filter isDigit) row
      where
        processElem :: Op -> String -> Int
        processElem Multiply "" = 1
        processElem Add "" = 0
        processElem _ xs = read xs

getInput :: IO [Column]
getInput = mapMaybe (toColumn . reverse) . transpose . map (unsafeParse lineParser) . lines <$> readFile "./fixtures/input6.txt"
  where
    toColumn :: [String] -> Maybe Column
    toColumn blah = do
      (h, t) <- uncons blah
      op <- toOp . head $ h
      return (Column op (map read t))

    lineParser :: Parser [String]
    lineParser = P.spaces *> (P.many1 (P.digit P.<|> P.oneOf "*+") `P.sepEndBy` P.spaces)

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res

toOp :: Char -> Maybe Op
toOp '*' = Just Multiply
toOp '+' = Just Add
toOp _ = Nothing

processColumn :: Column -> Int
processColumn (Column Multiply xs) = product xs
processColumn (Column Add xs) = sum xs