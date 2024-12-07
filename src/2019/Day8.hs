module Day8 where

import Data.List.Extra (minimumOn)
import Data.List.Split (chunksOf)

type Layer = [String]

part1 :: IO Int
part1 = (\x -> countDigits '1' x * countDigits '2' x) . minimumOn (countDigits '0') <$> getInput
  where
    countDigits digit = length . filter (== digit) . concat

-- The image you received is 25 pixels wide and 6 pixels tall.
parseInput :: String -> [Layer]
parseInput = chunksOf 6 . chunksOf 25

getInput :: IO [Layer]
getInput = parseInput <$> readFile "./fixtures/input8.txt"