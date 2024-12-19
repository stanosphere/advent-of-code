{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day10 where

import Control.Applicative (ZipList (ZipList, getZipList))
import Data.Foldable (find)
import Data.List
  ( findIndex,
    sort,
    tails,
    (\\),
  )
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import System.IO ()

part1 :: IO ()
part1 = do
  input <- getLines "./fixtures/input10.txt"
  print . sum . map getScorePart1 . mapMaybe findFirstCorruptedCharacter $ input

part2 :: IO ()
part2 = do
  input <- getLines "./fixtures/input10.txt"
  let completions = map getCompletion . mapMaybe getIncomplete $ input
  let scores = map getScoreForCompletion completions
  print . naiveMedian $ scores

getIncomplete :: String -> Maybe String
getIncomplete s =
  let reduced = eliminatePairs s
   in if reduced \\ "}]>)" == reduced then Just reduced else Nothing

getCompletion :: String -> String
getCompletion = map getCompliment . reverse

getScoreForCompletion :: String -> Integer
getScoreForCompletion = foldl (\acc elem -> acc * 5 + getScorePart2 elem) 0

doMatch :: Char -> Char -> Bool
doMatch '[' ']' = True
doMatch '(' ')' = True
doMatch '{' '}' = True
doMatch '<' '>' = True
doMatch _ _ = False

getCompliment :: Char -> Char
getCompliment '[' = ']'
getCompliment '(' = ')'
getCompliment '{' = '}'
getCompliment '<' = '>'

findFirstCorruptedCharacter :: String -> Maybe Char
findFirstCorruptedCharacter = find (`elem` "}]>)") . eliminatePairs

-- recursively eliminate pairs of matching brackets
eliminatePairs :: String -> String
eliminatePairs s = case findIndex (uncurry doMatch) . pairs $ s of
  Nothing -> s
  Just n -> eliminatePairs . deletePairAt n $ s

pointsTableForPart1 :: M.Map Char Int
pointsTableForPart1 = M.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

getScorePart1 :: Char -> Int
getScorePart1 = (pointsTableForPart1 M.!)

getScorePart2 :: Char -> Integer
getScorePart2 ')' = 1
getScorePart2 ']' = 2
getScorePart2 '}' = 3
getScorePart2 '>' = 4

-- ugly parsing stuff below here
getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)

toyInput :: [String]
toyInput =
  [ "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "(((({<>}<{<{<>}{[]{[]{}",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

-- random utility stuff here
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

pairs :: [a] -> [(a, a)]
pairs = map (\[x1, x2] -> (x1, x2)) . windows 2

deletePairAt :: Int -> [a] -> [a]
deletePairAt idx xs = let (start, _ : _ : end) = splitAt idx xs in start ++ end

naiveMedian :: [Integer] -> Integer
naiveMedian xs = sort xs !! ((length xs - 1) `div` 2)