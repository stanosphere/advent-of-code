module Day5 where

import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

{-
plan for part 1
- parse rules into a map of Int -> Set Int
- each entry will be the set of elements that ought to come after the key
- create a similar map for elements that ought to come before each key
- for each list iterate through and check that for each element, i
  - grab the elements before i and check that none of them appear in the "after" rule map under i
  - grab the elements after i and check that none of them appear in the "before" rule map under i
  - probably do set intersection for this actually!
  - at first I thought only one check would be needed but I'm pretty sure both are!
  - I really hope there's a simpler wya of doing this...
-}

data Rule = Rule {_before :: Int, _after :: Int} deriving (Show)

type RuleMap = M.Map Int (S.Set Int)

-- TODO naming is confusing ATM so I should use this!
data RuleMapping = RM {_beforeToAfter :: RuleMap, _afterToBefore :: RuleMap}

part1 :: IO Int
part1 = do
  (rules, lists) <- getInput
  let ruleMapping = toRuleMapping rules
  return . sum . map getMiddleElem . filter (isValid ruleMapping) $ lists

getMiddleElem :: [a] -> a
getMiddleElem xs = xs !! (length xs `div` 2)

isValid :: RuleMapping -> [Int] -> Bool
isValid rm xs = and . zipWith isElemValid [0 ..] $ xs
  where
    isElemValid index element =
      let elemsBefore = getElemsBefore index xs
          elemsThatShouldBeAfter = getElems element (_beforeToAfter rm)

          elemsAfter = getElemsAfter index xs
          elemsThatShouldBeBefore = getElems element (_afterToBefore rm)
       in (S.empty == S.intersection elemsBefore elemsThatShouldBeAfter)
            && (S.empty == S.intersection elemsAfter elemsThatShouldBeBefore)

getElems :: Int -> RuleMap -> S.Set Int
getElems x = fromMaybe S.empty . M.lookup x

getElemsBefore :: (Ord a) => Int -> [a] -> S.Set a
getElemsBefore n = S.fromList . take (n - 1)

getElemsAfter :: (Ord a) => Int -> [a] -> S.Set a
getElemsAfter n = S.fromList . drop (n + 1)

toRuleMapping :: [Rule] -> RuleMapping
toRuleMapping rules = RM (beforeToAfter rules) (afterToBefore rules)
  where
    beforeToAfter = M.map (S.fromList . map _after) . groupBy' _before
    afterToBefore = M.map (S.fromList . map _before) . groupBy' _after

getInput :: IO ([Rule], [[Int]])
getInput = parseInput . lines <$> readFile "./fixtures/input5.txt"

parseInput :: [String] -> ([Rule], [[Int]])
parseInput xs = (map parseRule . take 1176 $ xs, map parseList . drop 1177 $ xs)

-- parseInput :: [String] -> ([Rule], [[Int]])
-- parseInput xs = (map parseRule . take 21 $ xs, map parseList . drop 22 $ xs)

parseList :: String -> [Int]
parseList = map read . splitOn ","

parseRule :: String -> Rule
parseRule s =
  case splitOn "|" s of
    [before, after] -> Rule (read before) (read after)
    x -> error ("unexpected rule: " ++ show x)

-- this works like scala's groupBy in the sense that elements need not be adjacent in the original list to be grouped
groupBy' :: (Ord k) => (a -> k) -> [a] -> M.Map k [a]
groupBy' f =
  M.fromList
    . map (\xs -> (f . head $ xs, xs))
    . groupBy ((==) `on` f)
    . sortOn f