module Day3 where

import Data.List.Extra (find, uncons, unsnoc)
import Data.Maybe (mapMaybe)

-- part 1
-- - remove the last digit from the string
-- - for what's left find the biggest number
-- - get rid of everything before the biggest number (and the biggest number itself)
-- - add the last digit back in (might be the case that the biggest number is the penultimate one)
-- - find the biggest number
-- - you have your answer

-- part 2
-- - kind of the same as part 1 but you get rid of 11 digits, then 10, etc

data SolutionState = SolutionState
  { _found :: [Char], -- the digits we've found so far, intially empty list
    _start :: [Char], -- the start of the string were we get digits from
    _end :: [Char] -- digits we're not including in our search yet
  }

part1 :: IO Int
part1 = sum . map solve <$> getInput

part1' :: IO Int
part1' = sum . mapMaybe (solve' 2) <$> getInput

part2 :: IO Int
part2 = sum . mapMaybe (solve' 12) <$> getInput

solve' :: Int -> [Char] -> Maybe Int
solve' digits row = fmap (read . _found) . find ((== digits) . length . _found) . iterate step $ SolutionState [] start end
  where
    len = length row
    -- probably there's a splitAt or something?
    start = take (len - digits + 1) row
    end = drop (len - digits + 1) row

-- I kind of think vectors would be nicer because there's a lot of concatenations and un-concatenations going on
step :: SolutionState -> SolutionState
step (SolutionState found start end) =
  if null end
    then SolutionState (found ++ [nextFound]) [] []
    else SolutionState (found ++ [nextFound]) nextStart nextEnd
  where
    nextFound = maximum start
    (headEnd, nextEnd) = unsafeGet . uncons $ end
    nextStart = (++ [headEnd]) . drop 1 . dropWhile (/= nextFound) $ start

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
