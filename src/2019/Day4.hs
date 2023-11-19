module Day4 where

import Data.Char (digitToInt)
import Data.List (tails)

part1 :: Int
part1 = length . filter isValidPassword $ [193651 .. 649729]

part2 :: Int
part2 = length . filter isValidPassword' $ [193651 .. 649729]

isValidPassword :: Int -> Bool
isValidPassword i =
  all ($ i) [digitsNeverDecrease, hasTwoAdjacentDigitsTheSame]

isValidPassword' :: Int -> Bool
isValidPassword' i =
  all
    ($ i)
    [digitsNeverDecrease, hasTwoAdjacentDigitsTheSameThatAreNotPartOfLargerGroup]

hasTwoAdjacentDigitsTheSame :: Int -> Bool
hasTwoAdjacentDigitsTheSame =
  any (uncurry (==)) . pairs . map digitToInt . show

hasTwoAdjacentDigitsTheSameThatAreNotPartOfLargerGroup :: Int -> Bool
hasTwoAdjacentDigitsTheSameThatAreNotPartOfLargerGroup i =
  let digits = map digitToInt . show $ i
      candidatePairs =
        filter (\((_, x1), (_, x2)) -> x1 == x2) . pairs . zip [0 ..] $ digits
   in any
        ( \((i1, x1), (i2, x2)) ->
            leftSideIsFine digits i1 x1 && rightSideIsFine digits i2 x2
        )
        candidatePairs
  where
    leftSideIsFine :: [Int] -> Int -> Int -> Bool
    leftSideIsFine _ 0 _ = True
    leftSideIsFine vs i v = vs !! (i - 1) /= v
    rightSideIsFine :: [Int] -> Int -> Int -> Bool
    rightSideIsFine _ 5 _ = True
    rightSideIsFine vs i v = vs !! (i + 1) /= v

digitsNeverDecrease :: Int -> Bool
digitsNeverDecrease =
  all (\(left, right) -> right >= left) . pairs . map digitToInt . show

pairs :: [a] -> [(a, a)]
pairs = map (\[x1, x2] -> (x1, x2)) . windows 2

windows :: Int -> [a] -> [[a]]
windows n = takeWhile ((n ==) . length) . map (take n) . tails
