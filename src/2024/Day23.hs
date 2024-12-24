module Day23 where

import Data.List (sort)
import Data.List.Extra (nubOrd)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Tuple (swap)
import Utils.Grouping (groupMap)

part1 :: IO Int
part1 = do
  input <- getInput
  let nMap = mkNeighbourMap input
  return
    . length
    . nubOrd
    . concatMap (getTriples nMap)
    . filter ((== 't') . head)
    . allNodes
    $ input

getTriples :: M.Map String [String] -> String -> [(String, String, String)]
getTriples nMap node =
  [ sortTriple node x y
    | x <- nMap M.! node,
      y <- nMap M.! node,
      areConnected nMap x y
  ]

areConnected :: M.Map String [String] -> String -> String -> Bool
areConnected nMap x y = x `elem` nMap M.! y

allNodes :: [(String, String)] -> [String]
allNodes = nubOrd . concatMap (\(x, y) -> [x, y])

mkNeighbourMap :: [(String, String)] -> M.Map String [String]
mkNeighbourMap xs = groupMap fst snd . nubOrd $ (xs ++ map swap xs)

getInput :: IO [(String, String)]
getInput = map parseLine . lines <$> readFile "./fixtures/input23.txt"

parseLine :: String -> (String, String)
parseLine s = case splitOn "-" s of
  [x, y] -> (x, y)
  _ -> undefined

sortTriple :: Ord a => a -> a -> a -> (a, a, a)
sortTriple x y z =
  case sort [x, y, z] of
    [p, q, r] -> (p, q, r)
    _ -> undefined