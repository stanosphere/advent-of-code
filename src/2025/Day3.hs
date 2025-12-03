module Day3 where

import Data.List.Extra (unsnoc)

-- part 1
-- - remove the last digit from the string
-- - for what's left find the biggest number
-- - get rid of everything before the biggest number (and the biggest number itself)
-- - add the last digit back in (might be the case that the biggest number is the penultimate one)
-- - find the biggest number
-- - you have your answer

part1 :: IO Int
part1 = sum . map solve <$> getInput

solve :: String -> Int
solve s = read [tensDigit, unitsDigit]
  where
    (start, end) = unsafeGet . unsnoc $ s
    tensDigit = maximum start
    unitsDigit = maximum . (++ [end]) . drop 1 . dropWhile (/= tensDigit) $ start

unsafeGet :: Maybe a -> a
unsafeGet Nothing = undefined
unsafeGet (Just x) = x

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input3.txt"
