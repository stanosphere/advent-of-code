module Day7 where

import Data.List.Split (splitOn)
import qualified Data.Set as S

{-

plan for part 1

- using brute force would require 2 ^ (n -1) ops per line where n is the number of elems in the list
- so let's not
- instead I think I can accumulate a list of results and discard any that get too big as we go
- this should work since both (+) and (*) will increase the size of what I'm calculating
- and in most cases this will require far fewer than 2 ^ (n -1) ops
- I can't think of a suitable monoid for this but I think there will be a semigroup I can use
- so I should be able to use whatever `reduce` is called in Haskell...

-}

data Equation = Equation {_numbers :: [Int], _result :: Int} deriving (Show)

data EqState = EqState {_numberSet :: S.Set Int, _desiredResult :: Int} deriving (Show)

part1 :: IO Int
part1 = sum . map _desiredResult . filter isValid . map process <$> getInput

process :: Equation -> EqState
process eq = foldl updateState startingState xs
  where
    startingState = EqState (S.singleton . head . _numbers $ eq) (_result eq)
    xs = tail . _numbers $ eq

isValid :: EqState -> Bool
isValid (EqState numberSet desiredResult) = S.member desiredResult numberSet

updateState :: EqState -> Int -> EqState
updateState (EqState numberSet desiredResult) i = EqState res desiredResult
  where
    res = S.unions . S.map (\n -> S.filter (<= desiredResult) . S.fromList $ [n * i, n + i]) $ numberSet

getInput :: IO [Equation]
getInput = map parseInputLine . lines <$> readFile "./fixtures/input7.txt"

-- could use Parsec I suppose but input is so simple!
parseInputLine :: String -> Equation
parseInputLine s = Equation numbers (read result)
  where
    [result, numbersAsString] = splitOn ": " s
    numbers = map read . splitOn " " $ numbersAsString