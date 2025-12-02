module Day2 where

import Data.Foldable
import Data.Maybe (catMaybes)
import Text.Parsec as P (char, digit, many1, sepBy)
import Text.ParserCombinators.Parsec (Parser, parse)

-- ok so for part 1 I think I can do
-- - first transform the interval so that it's bounded by even numbers (you can't make an odd number out of repeated numbers)
-- - next work out the range for your two digit numbers
-- -- this can be done by inspecting the first half and second half of each bound in the interval
-- -- say we have an interval like abcd -> wxyz
-- -- we know our repeated numbers, r, must satisfy ab <= r <= wx
-- - we now have hopefully have a fairly small range to check and can do so exhaustively
-- - I guess it kind of square roots the search space, not exactly a square root but you know what I mean
-- - tbh it might be possible to brute force this without my fancy interval stuff but where's the fun in that?

data Interval = Interval {_start :: String, _end :: String} deriving (Show)

part1 = do
  input <- getInput
  let res = map reduceInterval input
  let res' = map (fmap getCandidates . reduceInterval) input
  let res'' = sum . concat . catMaybes $ res'
  let zipped = zip3 input res res'
  traverse_ print zipped
  print res''

-- this is horrible in terms of going to strings and back and so forth
getCandidates :: Interval -> [Int]
getCandidates (Interval start end) = filter (\c -> c >= startInt && c <= endInt) . map repeatOnce $ [getCandidate start .. getCandidate end]
  where
    repeatOnce :: Int -> Int
    repeatOnce = read . (\c -> show c ++ show c)
    getCandidate :: String -> Int
    getCandidate bound = read . take (length bound `div` 2) $ bound
    startInt = read start
    endInt = read end

-- reduces the interval if there are any odd numbers
-- and chucks out any intervals that we don't care about
reduceInterval :: Interval -> Maybe Interval
reduceInterval = check . modifyStartIfOdd . modifyEndIfOdd
  where
    modifyStartIfOdd i = if odd . length . _start $ i then Interval (roundUp . _start $ i) (_end i) else i
    modifyEndIfOdd i = if odd . length . _end $ i then Interval (_start i) (roundDown . _end $ i) else i
    check i = if (length . _start $ i) > (length . _end $ i) then Nothing else Just i

-- will round up a number to next power of 10
-- intended to make odd numbered lower bounds even
-- e.g. roundUp "982" == "1000"
roundUp :: String -> String
roundUp = show . (10 ^) . ceiling . logBase 10 . fromIntegral . read

-- will round up a number to next power of 10
-- intended to make odd numbered upper bounds even
-- roundDown "12345" == "9999"
roundDown :: String -> String
roundDown = show . (\x -> x - 1) . (10 ^) . floor . logBase 10 . fromIntegral . read

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