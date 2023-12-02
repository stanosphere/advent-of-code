module Day1 where

import Data.Char (digitToInt, intToDigit, isDigit)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (listToMaybe, mapMaybe)
import Prelude hiding (lookup)

-- 56506
part1 :: IO Int
part1 = fmap (sum . map getCalibration) . getLines $ "./fixtures/input1.txt"

-- 56017
part2 :: IO Int
part2 = fmap (sum . map getCalibration') . getLines $ "./fixtures/input1.txt"

getCalibration :: String -> Int
getCalibration s = read [head digits, last digits]
  where
    digits = filter isDigit s

getCalibration' :: String -> Int
getCalibration' s = read [intToDigit . head $ digits, intToDigit . last $ digits]
  where
    digits = toDigits s

data State = State {chars :: String, foundDigits :: [Int]}

toDigits :: Foldable t => t Char -> [Int]
toDigits = foundDigits . foldr f (State [] [])
  where
    f :: Char -> State -> State
    f char (State {chars, foundDigits})
      | isDigit char = State [] (digitToInt char : foundDigits)
      | length chars >= 2 = case maybeDigitString of
          Just digit -> State chars' (digit : foundDigits)
          Nothing -> State chars' foundDigits
      | otherwise = State chars' foundDigits
      where
        chars' = char : chars
        maybeDigitString = listToMaybe . mapMaybe ((`lookup` digitStrings) . (`take` chars')) $ [3, 4, 5]

digitStrings :: Map String Int
digitStrings = fromList (zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 ..])

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)