module Day6 where

import Data.List (transpose)
import Data.List.Extra (maximumOn, minimumOn)
import qualified Data.Map as M
import Utils.Grouping (frequencies)

part1 :: IO [Char]
part1 = map (fst . maximumOn snd . M.toList . frequencies) . transpose <$> getInput

part2 :: IO [Char]
part2 = map (fst . minimumOn snd . M.toList . frequencies) . transpose <$> getInput

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input6.txt"