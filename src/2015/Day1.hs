module Day1 where

import Data.List (find, scanl')
import Data.Maybe (fromJust)

data Move = Up | Down

part1 :: IO Int
part1 = foldr applyMove 0 <$> getInput

part2 :: IO Int
part2 =
  fst
    . fromJust
    . find ((== -1) . snd)
    . zip [0 ..]
    . scanl' (flip applyMove) 0
    <$> getInput

applyMove :: Move -> Int -> Int
applyMove Down = (+ (-1))
applyMove Up = (+ 1)

getInput :: IO [Move]
getInput = map toMove <$> readFile "./fixtures/input1.txt"
  where
    toMove '(' = Up
    toMove ')' = Down
    toMove _ = undefined