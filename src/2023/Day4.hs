{-# HLINT ignore "Use <$>" #-}
module Day4 where

import Data.Set qualified as S (Set, fromList, intersection, size)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

data Card = Card {cardId :: Int, winningNumbers :: S.Set Int, myNumbers :: S.Set Int} deriving (Show)

-- 28538, 0.03 secs
part1 :: IO Int
part1 = do
  inp <- getLines "./fixtures/input4.txt"
  return . sum . map (getScoreForCard . parseCard) $ inp

getScoreForCard :: Card -> Int
getScoreForCard (Card _ winningNumbers nyNumbers) = if winCount > 0 then 2 ^ (winCount - 1) else 0
  where
    winCount = S.size (S.intersection winningNumbers nyNumbers)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseCard :: String -> Card
parseCard = unsafeParse cardParser

cardParser :: Parser Card
cardParser = do
  P.string "Card" *> P.spaces
  cardId <- intParser
  P.char ':' *> P.spaces
  winners <- numbersParser
  P.char '|' *> P.spaces
  myNumbers <- numbersParser
  return (Card cardId winners myNumbers)
  where
    numbersParser = S.fromList <$> P.endBy intParser P.spaces
    intParser = read <$> P.many1 P.digit

unsafeParse :: Parser a -> String -> a
unsafeParse p s = case parse p "still don't really know wht this arg is for lol" s of
  Left res -> error . show $ res
  Right res -> res
