module Day12Part2 where

import Data.Char (isLower)
import Data.Foldable
import Data.List (group, iterate', nub, partition, sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Day12 (Cave, Edge (from, to), Path, appendPath, iterateUntil, largeToyExample, realInput, stringToEdge, toyExample)

data PathState = PathState {terminal :: [Path], nonTerminal :: [Path]} deriving (Show)

part2 :: Int
part2 = length . getAllPaths . map stringToEdge $ realInput

getAllPaths :: [Edge] -> [Path]
getAllPaths e = terminal . iterateUntil (null . nonTerminal) (iter e) $ PathState [] [["start"]]

iter :: [Edge] -> PathState -> PathState
iter edges pathState =
  let candidates =
        filter isValidPath
          . filter visitsStartOnce
          . concatMap (appendPath edges)
          . nonTerminal
          $ pathState
      (terminalPaths, nonTerminalPaths) = partition isTerminal candidates
   in PathState (terminalPaths ++ terminal pathState) nonTerminalPaths

visitsStartOnce :: Path -> Bool
visitsStartOnce = (== 1) . length . filter (== "start")

isValidPath :: Path -> Bool
isValidPath = isValidSmallCaveConfig . getSmallCavesOnly

isValidSmallCaveConfig :: [Cave] -> Bool
isValidSmallCaveConfig caves
  | any (> 2) smallCaveCounts = False
  | (> 1) . length . filter (== 2) $ smallCaveCounts = False
  | otherwise = True
  where
    smallCaveCounts = map length . group . sort $ caves

getSmallCavesOnly :: [Cave] -> [Cave]
getSmallCavesOnly = filter (\c -> c /= "start" && c /= "end" && all isLower c)

isTerminal :: Path -> Bool
isTerminal = (== "end") . head

reallySmallToyInput :: [String]
reallySmallToyInput =
  [ "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end"
  ]
