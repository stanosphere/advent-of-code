module Day7 where

import Data.List.Split (splitOn)
import Prelude hiding ((||))

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

data EqState = EqState {_numberSet :: [Int], _desiredResult :: Int} deriving (Show)

type Op = Int -> Int -> Int

part1 :: IO Int
part1 = solve [(*), (+)] <$> getInput

part2 :: IO Int
part2 = solve [(*), (+), (||)] <$> getInput

solve :: [Op] -> [Equation] -> Int
solve ops =
  sum
    . map _desiredResult
    . filter isValid
    . map (process ops)

process :: [Op] -> Equation -> EqState
process ops eqn = foldl (updateState ops) startingState xs
  where
    startingState = EqState [head . _numbers $ eqn] (_result eqn)
    xs = tail . _numbers $ eqn

isValid :: EqState -> Bool
isValid (EqState numberSet desiredResult) = desiredResult `elem` numberSet

updateState :: [Op] -> EqState -> Int -> EqState
updateState ops (EqState numberSet desiredResult) i = EqState newList desiredResult
  where
    newList = concatMap mkNewList numberSet
    mkNewList n = filter (<= desiredResult) . map (\op -> n `op` i) $ ops

(||) :: Int -> Int -> Int
x || y = read (show x ++ show y)

getInput :: IO [Equation]
getInput = map parseInputLine . lines <$> readFile "./fixtures/input7.txt"

-- could use Parsec I suppose but input is so simple!
parseInputLine :: String -> Equation
parseInputLine s = Equation numbers (read result)
  where
    [result, numbersAsString] = splitOn ": " s
    numbers = map read . splitOn " " $ numbersAsString