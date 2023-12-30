{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Day25 where

import Data.Foldable (traverse_)
import Data.List.Extra (nub, splitOn)

-- example graph has 33 edges
-- 33 C 3 is 5456
-- so I think one really could just check each possible triple in this case
-- however the real graph has 3317 edges
-- 3317 C 3 is 6,077,042,530
-- and I don't think doing 6 billion iterations is a good idea...

-- looks like Karger's algorithm will do this for us
-- https://en.wikipedia.org/wiki/Karger%27s_algorithm
-- but it requires random numbers which I've actually never used in Haskell
-- I'll also consult my algorithms book...
-- there are mentions of cuts in the Ford-Fulkerson section but that's more about network flows and such
-- and indeed because it's about flows that's all about directed graphs which isn't what we're dealing with here
-- Cuts are also mentioned in the minimum spanning tree section of my book
-- but I don't think that's useful for this problem
-- there's also the Stoer Wagner algorithm which might be a little easier to implement if less efficient than Karger's
-- https://en.wikipedia.org/wiki/Stoer%E2%80%93Wagner_algorithm
-- I can't see it mentioned in the book
part1 = do
  inp <- getLines "./fixtures/input25.txt"
  let graph = makeGraph inp
  traverse_ (\(a, b) -> putStrLn (a ++ " " ++ b)) graph
  print . length $ graph

makeGraph :: [String] -> [(String, String)]
makeGraph = nub . concatMap parseLine

parseLine :: String -> [(String, String)]
parseLine s =
  let [from, tos] = splitOn ": " s
      tos' = splitOn " " tos
   in map (sortEdge . (\x -> (from, x))) tos'

sortEdge :: (String, String) -> (String, String)
sortEdge (a, b) = if a < b then (a, b) else (b, a)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)