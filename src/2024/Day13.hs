module Day13 where

import Data.Maybe (mapMaybe)
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec (Parser, parse, try)

-- it costs 3 tokens to push the A button
-- and 1 token to push the B button

-- i think maybe lagrange multipliers is the way????
-- minimise something subject to something else
-- oh no I think they're actually not underdetermined at all are they
-- so let's just solve the simultaneous equations and see if there are integer solutions
-- being careful to detect if any are underdetermined or overdetermined just in case

-- so each input is a set of equations like like

-- z1 = y1 a + x2 b
-- z2 = y2 a + x2 b

-- yes I'm using weird notation, oh well!

-- the solutions (if they exist are then just)

-- a = (x2 z1 - x1 z2) / (y1 x2 - y2 x1)
-- b = (y1 z2 - y2 z1) / (y1 x2 - y2 x1)

data Equation = Eqn {z :: Int, y :: Int, x :: Int} deriving (Show)

data Solution = Sol {_a :: Int, _b :: Int} deriving (Show)

part1 = sum . map (\(Sol a b) -> 3 * a + b) . mapMaybe (uncurry solveEqns) <$> getInput

solveEqns :: Equation -> Equation -> Maybe Solution
solveEqns (Eqn z1 y1 x1) (Eqn z2 y2 x2)
  | denom == 0 = Nothing
  | aNumerator `mod` denom /= 0 = Nothing
  | aNumerator `mod` denom /= 0 = Nothing
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
      (y1, y2) <- firstLineParser
      _ <- P.newline
      (x1, x2) <- secondLineParser
      _ <- P.newline
      (z1, z2) <- thirdLineParser
      return (Eqn z1 y1 x1, Eqn z2 y2 x2)

    -- Button A: X+46, Y+68
    firstLineParser :: Parser (Int, Int)
    firstLineParser = do
      _ <- P.string "Button A: X+"
      y1 <- intParser
      _ <- P.string ", Y+"
      y2 <- intParser
      return (y1, y2)

    -- Button B: X+34, Y+14
    secondLineParser :: Parser (Int, Int)
    secondLineParser = do
      _ <- P.string "Button B: X+"
      x1 <- intParser
      _ <- P.string ", Y+"
      x2 <- intParser
      return (x1, x2)

    -- Prize: X=11306, Y=10856
    thirdLineParser :: Parser (Int, Int)
    thirdLineParser = do
      _ <- P.string "Prize: X="
      z1 <- intParser
      _ <- P.string ", Y="
      z2 <- intParser
      return (z1, z2)

    intParser :: Parser Int
    intParser = read <$> P.many1 P.digit
