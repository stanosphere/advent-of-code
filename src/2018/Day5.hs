module Day5 where

import Data.Char (toUpper)
import Data.Foldable (traverse_)

part1 :: IO ()
part1 = do
  inp <- readFile "./fixtures/input5.txt"
  let res = iterate annihilate inp
  traverse_ (print . length) . take 500 $ res

-- a bit brute forycy but does the trick; 11668 is the answer
annihilate :: String -> String
annihilate [] = []
annihilate [x] = [x]
annihilate (x : y : t) =
  if shouldAnnihilate x y
    then annihilate t
    else x : annihilate (y : t)

shouldAnnihilate :: Char -> Char -> Bool
shouldAnnihilate c1 c2 = c1 /= c2 && toUpper c1 == toUpper c2
