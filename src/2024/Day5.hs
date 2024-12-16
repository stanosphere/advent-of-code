module Day5 where

import Data.Function (on)
import Data.List (groupBy, sortBy, sortOn)
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

{-
plan for part 2

ok so I feel like ideally I'd make an an Ord instance but IDK how to do that like dynamically
so instead I think what I can do is make a function that assigns an Ordering based on a relevant subset of rules and then just use `sortBy`
supposing that this works it seems to me that I might be able to implement part1 in terms of part2
that is assuming that there is a unique correct ordering for each list in the input
which may not be the case, I think given the question  all that's required is all correct orderings have the same middle element which is a much weaker constraint than uniqueness
anyway I should probably like write some code now shouldn't I!
 -}

data Rule = Rule {_before :: Int, _after :: Int} deriving (Show)

type RuleMap = M.Map Int (S.Set Int)

data RuleMapping = RM {_beforeToAfter :: RuleMap, _afterToBefore :: RuleMap}

part1 :: IO Int
part1 = do
  (rules, lists) <- getInput
  let ruleMapping = toRuleMapping rules
  return . sum . map getMiddleElem . filter (isValid ruleMapping) $ lists

part2 :: IO Int
part2 = do
  (rules, lists) <- getInput
  let ruleMapping = toRuleMapping rules
  return . sum . map (getMiddleElem . sortList ruleMapping) . filter (not . isValid ruleMapping) $ lists

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

reduceRuleMapping :: S.Set Int -> RuleMapping -> RuleMapping
reduceRuleMapping xs rm = RM (reduceRuleMap . _beforeToAfter $ rm) (reduceRuleMap . _afterToBefore $ rm)
  where
    reduceRuleMap :: RuleMap -> RuleMap
    reduceRuleMap = M.map (S.intersection xs) . M.filterWithKey (\k _ -> S.member k xs)

-- hmmmmm looks like I only need half the mapping...
-- same is probably true in part one then!!!
createOrdering :: RuleMapping -> Int -> Int -> Ordering
createOrdering (RM beforeToAfter _) x y
  | x == y = EQ
  | S.member y . getElems x $ beforeToAfter = GT
  | otherwise = EQ

sortList :: RuleMapping -> [Int] -> [Int]
sortList rm xs = sortBy (createOrdering . reduceRuleMapping (S.fromList xs) $ rm) xs

toRuleMapping :: [Rule] -> RuleMapping
toRuleMapping rules = RM (beforeToAfter rules) (afterToBefore rules)
  where
    beforeToAfter = M.map (S.fromList . map _after) . groupBy' _before
    afterToBefore = M.map (S.fromList . map _before) . groupBy' _after

getInput :: IO ([Rule], [[Int]])
getInput = parseInput . lines <$> readFile "./fixtures/input5.txt"

parseInput :: [String] -> ([Rule], [[Int]])
parseInput xs = (map parseRule . take 1176 $ xs, map parseList . drop 1177 $ xs)

-- for the toy input, yes I hate hard coding the line numbers!
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