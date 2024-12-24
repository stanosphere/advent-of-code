module Day22 where

import Data.Bits (Bits (xor))
import Data.Char (digitToInt)
import Utils.Grouping (windows)

part1 :: IO Int
part1 = sum . map (nSteps 2000) <$> getInput

getInput :: IO [Int]
getInput = map read . lines <$> readFile "./fixtures/input22.txt"

nSteps :: Int -> Int -> Int
nSteps n x = iterate step x !! n

mkChangeSequence :: Int -> [Int]
mkChangeSequence = take 2000 . diffs . iterate step
  where
    diffs :: [Int] -> [Int]
    diffs = map (\y -> (lastDigit . last $ y) - (lastDigit . head $ y)) . windows 2

    lastDigit :: Int -> Int
    lastDigit = digitToInt . last . show

step :: Int -> Int
step = step3 . step2 . step1
  where
    step1 :: Int -> Int
    step1 x = prune . mix (64 * x) $ x

    step2 :: Int -> Int
    step2 x = prune . mix (floor (asDouble x / 32)) $ x

    step3 :: Int -> Int
    step3 x = prune . mix (2048 * x) $ x

    asDouble :: (Integral a) => a -> Double
    asDouble = fromIntegral

    mix :: Int -> Int -> Int
    mix = xor

    prune :: Int -> Int
    prune = (`mod` 16777216)
