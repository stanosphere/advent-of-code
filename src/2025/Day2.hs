module Day2 where

import Data.Containers.ListUtils (nubOrd)
import Text.Parsec as P (char, digit, many1, sepBy)
import Text.ParserCombinators.Parsec (Parser, parse)

data Interval = Interval {_start :: String, _end :: String} deriving (Show)

part1 :: IO Int
part1 =
  sum
    . nubOrd
    . concatMap (\i -> getCandidatesForFactor i ((`div` 2) . length . _start $ i))
    . filter (even . length . _start)
    . concatMap splitInterval
    <$> getInput

part2 :: IO Int
part2 =
  sum
    . nubOrd
    . concat
    . concatMap (\i -> map (getCandidatesForFactor i) . getFactors' $ i)
    . concatMap splitInterval
    <$> getInput

-- ok so for part 2 we have to generalize the way we get the invalid ids
-- first to simplify things I will split my interval into many intervals
-- each sub interval shall have the same number of digits to simplify things
-- (e.g. Interval 89 1234 -> [Interval 89 99, Interval 100 999, Interval 1000 1234])
-- now for each interval I will find the factors (excluding the number itself)
-- and then for each factor I can do what I did in part1 for each factor I find!

-- tbh using co-factors would have been a bit nicer here
getCandidatesForFactor :: Interval -> Int -> [Int]
getCandidatesForFactor (Interval start end) factor =
  filter (\c -> c >= startInt && c <= endInt)
    . map repeatAsRequired
    $ [getCandidate start .. getCandidate end]
  where
    repeatAsRequired :: Int -> Int
    repeatAsRequired = read . concat . (replicate (length start `div` factor) . show)
    getCandidate :: String -> Int
    getCandidate = read . take factor
    startInt = read start
    endInt = read end

-- e.g. Interval 89 1234 -> [Interval 89 99, Interval 100 999, Interval 1000 1234]
splitInterval :: Interval -> [Interval]
splitInterval (Interval start end) = zipWith Interval (start : mids) (map minusOne mids ++ [end])
  where
    startLen = length start
    endLen = length end
    mids = take (endLen - startLen) . iterate (\x -> x ++ ['0']) $ ('1' : replicate startLen '0')
    minusOne :: String -> String
    minusOne = show . (\x -> x - 1) . read

-- this will give us the number lengths we need to try
getFactors :: Int -> [Int]
getFactors n = filter ((== 0) . (n `mod`)) [1 .. n - 1]

getFactors' :: Interval -> [Int]
getFactors' = getFactors . length . _end

getInput :: IO [Interval]
getInput = unsafeParse inputParser <$> readFile "./fixtures/input2.txt"
  where
    inputParser :: Parser [Interval]
    inputParser = intervalParser `P.sepBy` P.char ','

    intervalParser :: Parser Interval
    intervalParser = Interval <$> P.many1 P.digit <* P.char '-' <*> P.many1 P.digit

    unsafeParse :: Parser a -> String -> a
    unsafeParse p s = case parse p "" s of
      Left res -> error . show $ res
      Right res -> res

-- NOTE: My original part 1 looked a bit like this
-- ok so for part 1 I think I can do
-- - first transform the interval so that it's bounded by even numbers (you can't make an odd number out of repeated numbers)
-- - next work out the range for your two digit numbers
-- -- this can be done by inspecting the first half and second half of each bound in the interval
-- -- say we have an interval like abcd -> wxyz
-- -- we know our repeated numbers, r, must satisfy ab <= r <= wx
-- - we now have hopefully have a fairly small range to check and can do so exhaustively
-- - I guess it kind of square roots the search space, not exactly a square root but you know what I mean
-- - tbh it might be possible to brute force this without my fancy interval stuff but where's the fun in that?