{-# HLINT ignore "Use <$>" #-}
module Day4 where

import Data.Foldable
import Data.Map qualified as M (Map, elems, fromList, size, (!))
import Data.Set qualified as S (Set, fromList, intersection, size)
import Text.Parsec qualified as P
import Text.ParserCombinators.Parsec (Parser, parse)

data Card = Card {cardId :: Int, winningNumbers :: S.Set Int, myNumbers :: S.Set Int} deriving (Show)

-- 28538, 0.03 secs
part1 :: IO Int
part1 = do
  inp <- getLines "./fixtures/input4.txt"
  return . sum . map (getScoreForCard . parseCard) $ inp

-- this naive approach takes 20 seconds
-- can definitely do better by making a sort of graph of card relationships
-- will be interesting to see how much better!
part2BruteForce :: IO (Maybe Int)
part2BruteForce = do
  inp <- getLines "./fixtures/input4.txt"
  return . fmap snd . find (null . fst) . bruteForce . M.fromList . map ((\c -> (cardId c, c)) . parseCard) $ inp

getScoreForCard :: Card -> Int
getScoreForCard c = if winCount > 0 then 2 ^ (winCount - 1) else 0
  where
    winCount = getWinCount c

getWinCount :: Card -> Int
getWinCount (Card _ winningNumbers nyNumbers) = S.size (S.intersection winningNumbers nyNumbers)

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

-- so let's brute force this to begin with...
-- I reckon we can sort of recurse through our stack of cards:
-- remove cards from the stack one by one and add the stuff they add to the stack and the result stack
-- actually the result stack can just be a counter...
bruteForce :: M.Map Int Card -> [([Card], Int)]
bruteForce cardMap = iterate step (M.elems cardMap, M.size cardMap)
  where
    step :: ([Card], Int) -> ([Card], Int)
    step ([], wins) = ([], wins)
    step (h : t, wins) = let newCards = getNewCards h in (newCards ++ t, wins + length newCards)
    getNewCards :: Card -> [Card]
    getNewCards c = map (cardMap M.!) . take (getWinCount c) $ [(1 + cardId c) ..]