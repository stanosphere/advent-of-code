{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day12 (iterateUntil, stringToEdge, toyExample, largeToyExample, realInput, Path, Edge (from, to), Cave, appendPath) where

import Data.Char (isLower)
import Data.List (nub, partition)
import Data.List.Split (splitOn)

type Cave = String

type Path = [Cave]

data PathState = PathState {terminal :: [Path], nonTerminal :: [Path]}

data Edge = Edge {from :: Cave, to :: Cave}

part1 :: Int
part1 = length . getAllPaths . map stringToEdge $ realInput

getAllPaths :: [Edge] -> [Path]
getAllPaths e = terminal . iterateUntil (null . nonTerminal) (iter e) $ PathState [] [["start"]]

iterateUntil :: (a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f a = if p a then a else iterateUntil p f (f a)

iter :: [Edge] -> PathState -> PathState
iter edges pathState =
  let candidates =
        filter (\x -> visitsSmallCaveLessThanTwice x && visitsStartOnce x)
          . concatMap (appendPath edges)
          . nonTerminal
          $ pathState
      (terminalPaths, nonTerminalPaths) = partition isTerminal candidates
   in PathState (terminalPaths ++ terminal pathState) nonTerminalPaths

appendPath :: [Edge] -> Path -> [Path]
appendPath edges p = nub (a ++ b)
  where
    a = map (\e -> to e : p) . filter ((== head p) . from) $ edges
    b = map (\e -> from e : p) . filter ((== head p) . to) $ edges

visitsStartOnce :: Path -> Bool
visitsStartOnce = (== 1) . length . filter (== "start")

visitsSmallCaveLessThanTwice :: Path -> Bool
visitsSmallCaveLessThanTwice p =
  let smallCaves = filter (\c -> c /= "start" && c /= "end" && all isLower c) p :: [Cave]
   in nub smallCaves == smallCaves

isTerminal :: Path -> Bool
isTerminal = (== "end") . head

-- parsing stuff
stringToEdge :: String -> Edge
stringToEdge s = case splitOn "-" s of [from, to] -> Edge from to

toyExample :: [String]
toyExample =
  [ "dc-end",
    "HN-start",
    "start-kj",
    "dc-start",
    "dc-HN",
    "LN-dc",
    "HN-end",
    "kj-sa",
    "kj-HN",
    "kj-dc"
  ]

largeToyExample :: [String]
largeToyExample =
  [ "fs-end",
    "he-DX",
    "fs-he",
    "start-DX",
    "pj-DX",
    "end-zg",
    "zg-sl",
    "zg-pj",
    "pj-he",
    "RW-he",
    "fs-DX",
    "pj-RW",
    "zg-RW",
    "start-pj",
    "he-WI",
    "zg-he",
    "pj-fs",
    "start-RW"
  ]

realInput :: [String]
realInput =
  [ "xz-end",
    "CJ-pt",
    "pt-QW",
    "hn-SP",
    "pw-CJ",
    "SP-end",
    "hn-pt",
    "GK-nj",
    "fe-nj",
    "CJ-nj",
    "hn-ZZ",
    "hn-start",
    "hn-fe",
    "ZZ-fe",
    "SP-nj",
    "SP-xz",
    "ZZ-pt",
    "nj-ZZ",
    "start-ZZ",
    "hn-GK",
    "CJ-end",
    "start-fe",
    "CJ-xz"
  ]