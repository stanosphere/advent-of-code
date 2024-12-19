module Day12 where

import Data.List (group)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)

-- so according to my calculations this should be doable vai brute force
-- would take 7,245,284 iterations for the real input
-- got this number by doing `print . sum . map ((2 ^) . length . filter (== '?') . fst . parseLine) $ xs`
-- 7857
-- (8.67 secs, 15,186,622,288 bytes)
part1 :: IO Int
part1 = do
  xs <- getLines "./fixtures/input12.txt"
  let res = map (uncurry countValid . parseLine) $ xs
  print res
  return . sum $ res

countValid :: [Int] -> String -> Int
countValid is = length . filter isValid . go
  where
    go :: String -> [String]
    go [] = [[]]
    go ('?' : t) = concatMap (\x -> ['#' : x, '.' : x]) (go t)
    go (h : t) = map (h :) (go t)
    isValid :: String -> Bool
    isValid s = is == (map length . filter (\x -> head x == '#') . group $ s)

parseLine :: String -> ([Int], String)
parseLine = unsafeParse lineParser

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "whatever" s of
  Left res -> error . show $ res
  Right res -> res

lineParser :: Parser ([Int], String)
lineParser = do
  res <- P.many springParser
  _ <- P.space
  numbers <- P.sepBy intParser (P.char ',')
  return (numbers, res)

springParser :: Parser Char
springParser = P.oneOf "#.?"

intParser :: Parser Int
intParser = read <$> P.many1 P.digit

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)