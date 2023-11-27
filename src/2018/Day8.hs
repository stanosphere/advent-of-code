{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day8 where

import Data.List.Extra ((!?))
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

toyInput :: [Int]
toyInput = map read . splitOn " " $ "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

-- "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
-- 2 nodes, 3 metadata at the end

-- 0 3 10 11 12

data Tree = Tree
  { meta :: [Int],
    children :: [Tree]
  }
  deriving (Show)

part1 :: IO ()
part1 = getRealInput >>= (print . sumTree . buildTree)

part2 :: IO ()
part2 = getRealInput >>= (print . sumTree' . buildTree)

sumTree :: Tree -> Int
sumTree t = (sum . meta $ t) + (sum . map sumTree . children $ t)

sumTree' :: Tree -> Int
sumTree' (Tree meta children) =
  if null children
    then sum meta
    else sum . mapMaybe (fmap sumTree' . (children !?)) $ meta

buildTree :: [Int] -> Tree
buildTree xs = head . fst . parseTree $ ([], xs)

parseTree :: ([Tree], [Int]) -> ([Tree], [Int])
parseTree (parsedSoFar, childCount : metaCount : rest) =
  let (parsedChildren, afterChildren) = iterateN childCount parseTree ([], rest)
      (meta, afterMeta) = splitAt metaCount afterChildren
      newNode = Tree meta parsedChildren
   in (newNode : parsedSoFar, afterMeta)

iterateN :: Int -> (a -> a) -> a -> a
iterateN n f a = iterate f a !! n

getRealInput :: IO [Int]
getRealInput = fmap (map read . splitOn " ") . readFile $ "./fixtures/input8.txt"

-- so someone wiser than I made a nice scala solution for this using something more akin to proper parser combinators
-- https://github.com/sim642/adventofcode/blob/master/src/main/scala/eu/sim642/adventofcode2018/Day8.scala
-- I bet I could translate that into haskell
-- looks like they've essentially implemented my iterateN as replicateParser