{-# OPTIONS_GHC -Wno-partial-fields #-}

module Day7Part2 where

import Data.Foldable
import Data.List
import Data.Map qualified as M (Map, alter, empty, toList)
import Data.Ord (Down (Down))

data Bid = Bid
  { hand :: Hand,
    cards :: [Card],
    money :: Int
  }
  deriving (Show, Eq)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Queen | King | Ace deriving (Eq, Ord, Show)

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
solve = do
  xs <- getLines "./fixtures/input7.txt"
  return . sum . zipWith (\i (Bid _ _ money) -> i * money) [1 ..] . sort . map parseBid $ xs

getBestSubstitution :: [Card] -> Hand
getBestSubstitution cs =
  let freqs = frequencies cs
      jokerCount = fmap (snd) . find ((== Joker) . fst) $ freqs
      res = case jokerCount of
        Nothing -> parseHand' freqs
        Just x -> getBestSub x cs
   in res

getBestSub :: Int -> [Card] -> Hand
getBestSub count = maximum . map (parseHand' . frequencies) . getPossibleSubs count

getPossibleSubs :: Int -> [Card] -> [[Card]]
getPossibleSubs count cards =
  let candidates = nub cards
      res
        | count == 1 = [[c] | c <- candidates]
        | count == 2 = [[c1, c2] | c1 <- candidates, c2 <- candidates]
        | count == 3 = [[c1, c2, c3] | c1 <- candidates, c2 <- candidates, c3 <- candidates]
        | count == 4 = [replicate 5 (head candidates)]
        | otherwise = [replicate 5 Ace]
   in res

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

parseBid :: String -> Bid
parseBid s = Bid (getBestSubstitution . map parseCard . take 5 $ s) (map parseCard . take 5 $ s) (read . drop 6 $ s)

parseHand' :: (Eq a1, Num a1) => [(a2, a1)] -> Hand
parseHand' [(_, 5)] = FiveOfAKind
parseHand' [(_, 4), (_, 1)] = FourOfAKind
parseHand' [(_, 3), (_, 2)] = FullHouse
parseHand' ((_, 3) : _) = ThreeOfAKind
parseHand' [(_, 2), (_, 2), (_, 1)] = TwoPair
parseHand' ((_, 2) : _) = OnePair
parseHand' _ = HighCard

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = sortOn (Down . snd) . M.toList . foldr (alter' (maybe 1 (+ 1))) M.empty
  where
    -- like alter but can't delete elements
    alter' :: Ord k => (Maybe a -> a) -> k -> M.Map k a -> M.Map k a
    alter' f = M.alter (Just . f)

instance Ord Bid where
  compare :: Bid -> Bid -> Ordering
  compare left right
    | leftHand < rightHand = LT
    | leftHand > rightHand = GT
    | otherwise = compare (cards left) (cards right)
    where
      leftHand = hand left
      rightHand = hand right

parseCard :: Char -> Card
parseCard 'J' = Joker
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = Ten
parseCard 'Q' = Queen
parseCard 'K' = King
parseCard 'A' = Ace
parseCard _ = undefined
