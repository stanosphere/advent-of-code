module Day2 where

import Data.Map qualified as M (Map, elems, findWithDefault, fromList, unionWith)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

-- could also use a more precise datatype if I wanted I suppose
-- data Hand = Hand {red :: Int, green :: Int, blue :: Int}
type Hand = M.Map String Int

data Game = Game {gameId :: Int, hands :: [Hand]} deriving (Show)

-- 2156
part1 :: IO Int
part1 = sum . map gameId . filter gameIsPossible . map (unsafeParse gameParser) <$> getLines "./fixtures/input2.txt"

-- 66909
part2 :: IO Int
part2 = sum . map (gamePower . unsafeParse gameParser) <$> getLines "./fixtures/input2.txt"

gamePower :: Game -> Int
gamePower = product . M.elems . foldl1 (M.unionWith max) . hands

gameIsPossible :: Game -> Bool
gameIsPossible = all handIsPossible . hands

-- only 12 red cubes, 13 green cubes, and 14 blue cubes
handIsPossible :: Hand -> Bool
handIsPossible hand = (getCount "red" <= 12) && (getCount "green" <= 13) && (getCount "blue" <= 14)
  where
    getCount colour = M.findWithDefault 0 colour hand

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

-- Parsing stuff below here
-- https://jsdw.me/posts/haskell-parsec-basics/

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "still don't really know wht this arg is for lol" s of
  Left res -> error . show $ res
  Right res -> res

gameParser :: Parser Game
gameParser = do
  P.string "Game "
  gameId <- intParser
  P.char ':'
  P.spaces
  res <- P.sepBy handParser (P.string "; ")
  return (Game gameId res)

handParser :: Parser Hand
handParser = M.fromList <$> P.sepBy cubeCountParser (P.string ", ")

cubeCountParser :: Parser (String, Int)
cubeCountParser = do
  digits <- intParser
  P.spaces
  letters <- P.many1 P.letter
  return (letters, digits)

intParser :: Parser Int
intParser = read <$> P.many1 P.digit
