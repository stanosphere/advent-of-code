module Day1 where

import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.List (scanl')
import qualified Data.Set as S
import Text.Parsec as P (char, digit, many1, parse, sepBy, string, (<|>))
import Text.ParserCombinators.Parsec (Parser)

data Turn = L | R deriving (Show, Eq)

data Move = Move Turn Int deriving (Show)

data Move' = Advance | Rotate Turn deriving (Show, Eq)

data Orientation = N | E | S | W deriving (Show)

type Coord = (Int, Int)

data Position = Position Coord Orientation deriving (Show)

part1 :: IO Int
part1 =
  dist
    . foldl' applyMove (Position (0, 0) N)
    <$> getInput

part2 :: IO (Maybe Int)
part2 = do
  moves <- expandMoves <$> getInput

  let allPositions =
        map (\(Position c _) -> c)
          . scanl' applyMove' (Position (0, 0) N)
          $ moves

  return
    . fmap (\(x, y) -> abs x + abs y)
    . firstToAppearTwice
    . map snd
    . filter ((== Advance) . fst)
    $ zip moves allPositions

dist :: Position -> Int
dist (Position (x, y) _) = abs x + abs y

expandMoves :: [Move] -> [Move']
expandMoves = concatMap expandMove
  where
    expandMove :: Move -> [Move']
    expandMove (Move t i) = Rotate t : replicate i Advance

firstToAppearTwice :: Ord a => [a] -> Maybe a
firstToAppearTwice = go S.empty
  where
    go _ [] = Nothing
    go seen (x : xs) = if S.member x seen then Just x else go (S.insert x seen) xs

applyMove' :: Position -> Move' -> Position
applyMove' p Advance = advance 1 p
applyMove' p (Rotate t) = turn t p

applyMove :: Position -> Move -> Position
applyMove p (Move direction steps) = advance steps . turn direction $ p

advance :: Int -> Position -> Position
advance i (Position (x, y) N) = Position (x, y - i) N
advance i (Position (x, y) E) = Position (x + i, y) E
advance i (Position (x, y) S) = Position (x, y + i) S
advance i (Position (x, y) W) = Position (x - i, y) W

turn :: Turn -> Position -> Position
turn t (Position c o) = Position c (turn' t o)
  where
    turn' :: Turn -> Orientation -> Orientation
    turn' R N = E
    turn' R E = S
    turn' R S = W
    turn' R W = N
    turn' L N = W
    turn' L E = N
    turn' L S = E
    turn' L W = S

getInput :: IO [Move]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input1.txt"

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "" s of
  Left res -> error . show $ res
  Right res -> res

inputParser :: Parser [Move]
inputParser = moveParser `sepBy` string ", "
  where
    moveParser = Move <$> turnParser <*> (read <$> many1 digit)
    turnParser = (char 'L' $> L) <|> (char 'R' $> R)
