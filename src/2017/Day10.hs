module Day10 where

import Data.List.Extra (foldl', splitOn)

data StringState = StringState
  { _string :: [Int],
    _skipSize :: Int,
    _position :: Int
  }
  deriving (Show)

part1 = product . take 2 . _string . foldl' step initState <$> getInput
  where
    initState = StringState [0 .. 255] 0 0

step :: StringState -> Int -> StringState
step (StringState string skipSize position) len = StringState string' skipSize' position'
  where
    stringLength = length string
    (left, right) = splitAt position string
    reversed = reverse . take len $ (right ++ left)
    leftAlone = drop len (right ++ left)
    string' = take stringLength . drop (stringLength - position) . cycle $ (reversed ++ leftAlone)
    position' = (position + len + skipSize) `mod` stringLength
    skipSize' = skipSize + 1

getInput :: IO [Int]
getInput = map read . splitOn "," <$> readFile "./fixtures/input10.txt"