module Day19 where

import Control.Monad.State (State, evalState, gets, modify)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as M

type TowelPattern = String

type TowelDesign = String

type Cache = M.Map TowelDesign Int

part1 :: IO Int
part1 = do
  (patterns, designs) <- getInput
  return . length . filter (isPossible patterns) $ designs

part2 :: IO Int
part2 = do
  (patterns, designs) <- getInput
  let res = fmap sum . traverse (countWays patterns) $ designs
  let res' = evalState res M.empty
  return res'

countWays :: [TowelPattern] -> TowelDesign -> State Cache Int
countWays _ [] = return 1
countWays patterns design = fmap sum . traverse countRemainingWays . filter (`isPrefixOf` design) $ patterns
  where
    countRemainingWays pattern =
      let remainingString = drop (length pattern) design
       in getOrUpdate remainingString (countWays patterns remainingString)

-- using state monad to maintain a cache of stuff I've already found
-- the stuff in the state will only actually get evaluated if we don't find anything in the cache
getOrUpdate :: (Ord k) => k -> State (M.Map k v) v -> State (M.Map k v) v
getOrUpdate k state = do
  maybeVal <- gets (M.lookup k)
  case maybeVal of
    Just v -> return v
    Nothing -> do
      valueToInsert <- state
      modify (M.insert k valueToInsert)
      return valueToInsert

isPossible :: [TowelPattern] -> TowelDesign -> Bool
isPossible _ [] = True
isPossible patterns design = any isPossible' patterns
  where
    isPossible' :: TowelPattern -> Bool
    isPossible' pattern = pattern `isPrefixOf` design && isPossible patterns (drop (length pattern) design)

getInput :: IO ([TowelPattern], [TowelDesign])
getInput = parseInput . lines <$> readFile "./fixtures/input19.txt"

parseInput :: [String] -> ([TowelPattern], [TowelDesign])
parseInput xs = (patterns, designs)
  where
    patterns = splitOn ", " . head $ xs
    designs = drop 2 xs