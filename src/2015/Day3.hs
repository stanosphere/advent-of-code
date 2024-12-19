module Day3 where

import Data.Bifunctor (Bifunctor, bimap)
import Data.List (partition)
import Data.List.Extra (nubOrd)
import Text.Parsec as P (char, choice, many1)
import Text.ParserCombinators.Parsec (Parser, parse)

type Coord = (Int, Int)

data Direction = U | D | L | R deriving (Show, Eq, Ord)

part1 :: IO Int
part1 =
  length
    . nubOrd
    . scanl move (0, 0)
    . unsafeParse inputParser
    <$> readFile "./fixtures/input3.txt"

part2 :: IO Int
part2 =
  length
    . nubOrd
    . uncurry (++)
    . mapBoth (scanl move (0, 0) . map snd)
    . partition (even . fst)
    . zip [0 ..]
    . unsafeParse inputParser
    <$> readFile "./fixtures/input3.txt"

mapBoth :: (Bifunctor bf) => (a -> b) -> bf a a -> bf b b
mapBoth f = bimap f f

move :: Coord -> Direction -> Coord
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [Direction]
inputParser = many1 directionParser
  where
    directionParser = choice [L <$ char '<', R <$ char '>', U <$ char '^', D <$ char 'v']
