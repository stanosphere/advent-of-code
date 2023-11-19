module Day5 where

import Data.Char (toUpper)

part1 :: IO ()
part1 = do
  inp <- readFile "./fixtures/input5.txt"
  let res = iteratetoFixedPoint annihilate inp
  print . length $ res

-- a bit brute forycy but does the trick; 11668 is the answer
annihilate :: String -> String
annihilate [] = []
annihilate [x0] = [x0]
annihilate (x0 : x1 : xs)
  | shouldAnnihilate x0 x1 = annihilate xs
  | otherwise = x0 : annihilate (x1 : xs)

iteratetoFixedPoint :: Eq a => (a -> a) -> a -> a
iteratetoFixedPoint f x
  | x' == x = x'
  | otherwise = iteratetoFixedPoint f x'
  where
    x' = f x

shouldAnnihilate :: Char -> Char -> Bool
shouldAnnihilate c1 c2 = c1 /= c2 && toUpper c1 == toUpper c2
