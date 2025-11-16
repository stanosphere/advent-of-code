module Day4 where

import Data.List (sort)
import qualified Data.Set as S
  ( Set,
    empty,
    insert,
    member,
  )

part1 :: IO Int
part1 = length . filter rowHasNoDuplicates <$> getInput

part2 :: IO Int
part2 = length . filter rowHasNoAnagrams <$> getInput

-- could probably do something fancier like the prime number anagram algo
-- but using sort is fine for such a small input
rowHasNoAnagrams :: [String] -> Bool
rowHasNoAnagrams = rowHasNoDuplicates . map sort

-- this could probably be prettier if I used guards
-- or tbh the input is so small I could just compare length to length . nubOrd or whatever, right?
rowHasNoDuplicates :: [String] -> Bool
rowHasNoDuplicates =
  snd
    . foldl
      ( \(s, b) x ->
          if not b
            then (s, b)
            else
              if S.member x s
                then (s, False)
                else (S.insert x s, b)
      )
      zero
  where
    zero :: (S.Set String, Bool)
    zero = (S.empty, True)

getInput :: IO [[String]]
getInput = map words . lines <$> readFile "./fixtures/input4.txt"