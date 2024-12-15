module Day2 where

import Data.Map
  ( Map,
    alter,
    elems,
    empty,
    lookup,
  )
import Data.Maybe (catMaybes)
import Prelude hiding (lookup)

part1 :: IO ()
part1 = do
  xs <- getLines "./fixtures/input2.txt"
  print . checksum $ xs

part2 :: IO ()
part2 = do
  xs <- getLines "./fixtures/input2.txt"
  print . uncurry showCommonElems . head . getMatches $ xs

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

checksum :: (Ord k) => [[k]] -> Int
checksum xs =
  let twos = length . filter (hasExactlyNElems 2) $ xs
      threes = length . filter (hasExactlyNElems 3) $ xs
   in twos * threes

-- hasExactlyNLetters :: Int -> String -> Bool
hasExactlyNElems :: (Ord k) => Int -> [k] -> Bool
hasExactlyNElems n = elem n . elems . frequencies

frequencies :: (Ord a) => [a] -> Map a Int
frequencies = foldr (alter' (maybe 1 (+ 1))) empty

-- like alter but can't delete elements
alter' :: (Ord k) => (Maybe a -> a) -> k -> Map k a -> Map k a
alter' f = alter (Just . f)

getMatches :: (Eq b) => [[b]] -> [([b], [b])]
getMatches xs = [(s1, s2) | s1 <- xs, s2 <- xs, differsBySingleElem s1 s2]

differsBySingleElem :: (Eq b) => [b] -> [b] -> Bool
differsBySingleElem s1 s2 = lookup False (frequencies $ zipWith (==) s1 s2) == Just 1

showCommonElems :: (Eq a) => [a] -> [a] -> [a]
showCommonElems s1 s2 = catMaybes (zipWith whenEqual s1 s2)

whenEqual :: (Eq a) => a -> a -> Maybe a
whenEqual x1 x2 = if x1 == x2 then Just x1 else Nothing