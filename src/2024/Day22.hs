module Day22 where

import Data.Bits (Bits (xor))

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
