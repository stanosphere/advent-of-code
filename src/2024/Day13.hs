module Day13 where

import Data.Bifunctor (bimap)
import Data.Maybe (mapMaybe)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse)

-- so each input is a set of equations like

-- z1 = y1 a + x2 b
-- z2 = y2 a + x2 b

-- yes I'm using weird notation, oh well!

-- the solutions (if they exist are then just)

-- a = (x2 z1 - x1 z2) / (y1 x2 - y2 x1)
-- b = (y1 z2 - y2 z1) / (y1 x2 - y2 x1)

data Equation = Eqn {_z :: Int, _y :: Int, _x :: Int} deriving (Show)

data Solution = Sol {_a :: Int, _b :: Int} deriving (Show)

part1 :: IO Int
part1 =
  sum
    . map (\(Sol a b) -> 3 * a + b)
    . mapMaybe (uncurry solveEqns)
    <$> getInput

part2 :: IO Int
part2 =
  sum
    . map (\(Sol a b) -> 3 * a + b)
    . mapMaybe (uncurry solveEqns . bimap addBigOffset addBigOffset)
    <$> getInput

addBigOffset :: Equation -> Equation
addBigOffset (Eqn z y x) = Eqn (10000000000000 + z) y x

solveEqns :: Equation -> Equation -> Maybe Solution
solveEqns (Eqn z1 y1 x1) (Eqn z2 y2 x2)
  | denom == 0 = Nothing
  | aNumerator `mod` denom /= 0 = Nothing
  | bNumerator `mod` denom /= 0 = Nothing
  | otherwise = Just (Sol (aNumerator `div` denom) (bNumerator `div` denom))
  where
    denom = y1 * x2 - y2 * x1
    aNumerator = x2 * z1 - x1 * z2
    bNumerator = y1 * z2 - y2 * z1

getInput :: IO [(Equation, Equation)]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input13.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "this arg is the source name, I guess it's just for error messages or something?" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [(Equation, Equation)]
inputParser = P.sepBy equationParser (P.newline *> P.newline)
  where
    equationParser :: Parser (Equation, Equation)
    equationParser = do
      (y1, y2) <- firstLineParser <* P.newline
      (x1, x2) <- secondLineParser <* P.newline
      (z1, z2) <- thirdLineParser
      return (Eqn z1 y1 x1, Eqn z2 y2 x2)

    -- Button A: X+46, Y+68
    firstLineParser :: Parser (Int, Int)
    firstLineParser = do
      y1 <- P.string "Button A: X+" *> intParser
      y2 <- P.string ", Y+" *> intParser
      return (y1, y2)

    -- Button B: X+34, Y+14
    secondLineParser :: Parser (Int, Int)
    secondLineParser = do
      x1 <- P.string "Button B: X+" *> intParser
      x2 <- P.string ", Y+" *> intParser
      return (x1, x2)

    -- Prize: X=11306, Y=10856
    thirdLineParser :: Parser (Int, Int)
    thirdLineParser = do
      z1 <- P.string "Prize: X=" *> intParser
      z2 <- P.string ", Y=" *> intParser
      return (z1, z2)

    intParser :: Parser Int
    intParser = read <$> P.many1 P.digit
