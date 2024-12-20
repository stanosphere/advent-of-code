module Day5 where

import Data.List (sort)
import qualified Data.Map as M
import Utils.Grouping (groupMap, windows)

part1 :: IO Int
part1 = length . filter isNice <$> getInput

part2 :: IO Int
part2 = length . filter isNice' <$> getInput

isNice :: String -> Bool
isNice s = all ($ s) [hasAtLeastThreeVowels, containsDoubleLetter, containsNoNastyStrings]
  where
    hasAtLeastThreeVowels :: String -> Bool
    hasAtLeastThreeVowels = (== 3) . length . take 3 . filter (`elem` "aeiou")

    containsDoubleLetter :: String -> Bool
    containsDoubleLetter = any (\x -> head x == last x) . windows 2

    containsNoNastyStrings :: String -> Bool
    containsNoNastyStrings = all (`notElem` ["ab", "cd", "pq", "xy"]) . windows 2

isNice' :: String -> Bool
isNice' s = all ($ s) [pairOfTwoLetters, repeatsWithLetterBetween]
  where
    repeatsWithLetterBetween :: String -> Bool
    repeatsWithLetterBetween = any (\x -> head x == last x) . windows 3

    pairOfTwoLetters :: String -> Bool
    pairOfTwoLetters =
      not
        . M.null
        . M.filter (any (\x -> last x > head x + 1) . windows 2 . sort)
        . M.filter ((>= 2) . length)
        . groupMap (map snd) (fst . head)
        . windows 2
        . zip [(0 :: Int) ..]

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input5.txt"
