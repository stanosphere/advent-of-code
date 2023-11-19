module Day5 where

import Data.Char (toUpper)

part1 :: IO ()
part1 = do
  inp <- readFile "./fixtures/input5.txt"
  print . length . iteratetoFixedPoint annihilate $ inp

-- I've checked and it's all letters, a->z that appear in the input string
part2 :: IO ()
part2 = do
  inp <- readFile "./fixtures/input5.txt"
  print . minimum . map (`attemptForUnit` inp) $ ['a' .. 'z']

removeUnit :: Char -> String -> String
removeUnit unit = filter (\x -> toUpper x /= toUpper unit)

attemptForUnit :: Char -> String -> Int
attemptForUnit unit = length . iteratetoFixedPoint annihilate . removeUnit unit

-- a bit brute forcy but does the trick; 11668 is the answer
annihilate :: String -> String
annihilate [] = []
annihilate [x0] = [x0]
annihilate (x0 : x1 : xs)
  | shouldAnnihilate x0 x1 = annihilate xs
  | otherwise = x0 : annihilate (x1 : xs)
  where
    shouldAnnihilate :: Char -> Char -> Bool
    shouldAnnihilate c1 c2 = c1 /= c2 && toUpper c1 == toUpper c2

iteratetoFixedPoint :: Eq a => (a -> a) -> a -> a
iteratetoFixedPoint f x
  | x' == x = x'
  | otherwise = iteratetoFixedPoint f x'
  where
    x' = f x
