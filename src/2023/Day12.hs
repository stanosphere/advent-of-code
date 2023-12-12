module Day12 where

import Data.Foldable
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

part1 = do
  xs <- getLines "./fixtures/input12.txt"
  print . sum . map ((2 ^) . length . filter (== '?') . fst . parseLine) $ xs

--   traverse_ print ys
-- so according to my calculations this should be doable vai brute force
-- would take 7,245,284 iterations for the real input
-- got this number by doing `print . sum . map ((2 ^) . length . filter (== '?') . fst . parseLine) $ xs`

go :: String -> [String]
go [] = [[]]
go ('?' : t) = concatMap (\x -> ['#' : x, '.' : x]) (go t)
go (h : t) = map (h :) (go t)

parseLine :: String -> (String, [Int])
parseLine = unsafeParse lineParser

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

lineParser :: Parser (String, [Int])
lineParser = do
  res <- P.many springParser
  _ <- P.space
  numbers <- P.sepBy intParser (P.char ',')
  return (res, numbers)

springParser :: Parser Char
springParser = P.oneOf "#.?"

intParser :: Parser Int
intParser = read <$> P.many1 P.digit

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)