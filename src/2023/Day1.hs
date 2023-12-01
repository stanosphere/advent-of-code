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

data State = State {chars :: String, foundDigits :: [Int]} deriving (Show)

toDigits :: Foldable t => t Char -> [Int]
toDigits = foundDigits . foldl f (State [] [])
  where
    f :: State -> Char -> State
    f (State {chars, foundDigits}) char
      | isDigit char = State [] (foundDigits ++ [digitToInt char])
      | length chars >= 2 = case maybeDigitString of
          Just digit -> State chars' (foundDigits ++ [digit])
          Nothing -> State chars' foundDigits
      | otherwise = State chars' foundDigits
      where
        chars' = chars ++ [char]
        maybeDigitString = listToMaybe . mapMaybe ((`lookup` digitStrings) . (`takeRight` chars')) $ [3, 4, 5]

takeRight :: Int -> [a] -> [a]
takeRight n = reverse . take n . reverse

digitStrings :: Map String Int
digitStrings = fromList (zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 ..])

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)