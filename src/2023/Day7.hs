{-# OPTIONS_GHC -Wno-partial-fields #-}

module Day7 where

import Data.List (sort, sortOn)
import qualified Data.Map as M (Map, alter, empty, toList)
import Data.Ord (Down (Down))

data Bid = Bid {hand :: Hand, cards :: [Card], money :: Int} deriving (Show, Eq)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Show)

data Hand
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | FullHouse
  | FourOfAKind
  | FiveOfAKind
  deriving (Eq, Show, Ord)

-- 249483956
-- 0.05 secs
solve :: IO Int
solve = do
  xs <- getLines "./fixtures/input7.txt"
  return . sum . zipWith (\i (Bid _ _ money) -> i * money) [1 ..] . sort . map parseBid $ xs

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseBid :: String -> Bid
parseBid s = Bid (parseHand . take 5 $ s) (map parseCard . take 5 $ s) (read . drop 6 $ s)

parseHand :: [Char] -> Hand
parseHand = parseHand' . frequencies . map parseCard
  where
    parseHand' [(_, 5)] = FiveOfAKind
    parseHand' [(_, 4), (_, 1)] = FourOfAKind
    parseHand' [(_, 3), (_, 2)] = FullHouse
    parseHand' ((_, 3) : _) = ThreeOfAKind
    parseHand' [(_, 2), (_, 2), (_, 1)] = TwoPair
    parseHand' ((_, 2) : _) = OnePair
    parseHand' _ = HighCard
    frequencies :: (Ord a) => [a] -> [(a, Int)]
    frequencies = sortOn (Down . snd) . M.toList . foldr (alter' (maybe 1 (+ 1))) M.empty
    -- like alter but can't delete elements
    alter' :: (Ord k) => (Maybe a -> a) -> k -> M.Map k a -> M.Map k a
    alter' f = M.alter (Just . f)

instance Ord Bid where
  compare left right
    | leftHand < rightHand = LT
    | leftHand > rightHand = GT
    | otherwise = compare (cards left) (cards right)
    where
      leftHand = hand left
      rightHand = hand right

parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = Ten
parseCard 'J' = Jack
parseCard 'Q' = Queen
parseCard 'K' = King
parseCard 'A' = Ace
parseCard _ = undefined
