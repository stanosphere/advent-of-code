module Day9 where

import Data.List.Extra (nubOrd, permutations)
import qualified Data.Map as M
import Text.Parsec as P (digit, letter, many1, newline, sepBy, string)
import Text.ParserCombinators.Parsec (Parser, parse)
import Utils.Grouping (windows)

data Edge = Edge String String Int deriving (Show)

type EdgeMap = M.Map (String, String) Int

-- only 8 nodes so 40320 combos
part1 :: IO Int
part1 =
  minimum
    . getAllRouteWeights
    . unsafeParse inputParser
    <$> readFile "./fixtures/input9.txt"

part2 :: IO Int
part2 =
  maximum
    . getAllRouteWeights
    . unsafeParse inputParser
    <$> readFile "./fixtures/input9.txt"

getAllRouteWeights :: [Edge] -> [Int]
getAllRouteWeights edges = map (getRouteWeight edgeMap) . permutations $ nodes
  where
    nodes = nubOrd . concatMap (\(Edge x y _) -> [x, y]) $ edges
    edgeMap = M.fromList . concatMap (\(Edge x y v) -> [((x, y), v), ((y, x), v)]) $ edges

getRouteWeight :: EdgeMap -> [String] -> Int
getRouteWeight edgeMap = sum . map (edgeMap M.!) . pairs

pairs :: [a] -> [(a, a)]
pairs = map asTuple . windows 2
  where
    asTuple :: [a] -> (a, a)
    asTuple list = (head list, last list)

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [Edge]
inputParser = lineParser `sepBy` newline

lineParser :: Parser Edge
lineParser =
  Edge
    <$> (many1 letter <* string " to ")
    <*> (many1 letter <* string " = ")
    <*> (read <$> many1 digit)
