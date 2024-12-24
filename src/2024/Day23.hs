module Day23 where

import Data.List (intercalate, sort)
import Data.List.Extra (maximumOn, nubOrd)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)
import Utils.Grouping (groupMap)

-- for part 2 I use the pivoting version of the Bron–Kerbosch algorithm https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
-- I never saw the non-pivoting version finish
part2 :: IO ()
part2 = do
  input <- getInput
  let nMap = mkNeighbourSetMap input
  let ns = allNodes input
  let allCliques = bk2 nMap [] S.empty (S.fromList ns) S.empty
  putStrLn . intercalate "," . sort . S.toList . maximumOn S.size $ allCliques

-- could maybe use state monad or something to store my maximals???
bk2 ::
  M.Map String (S.Set String) ->
  [S.Set String] ->
  S.Set String ->
  S.Set String ->
  S.Set String ->
  [S.Set String]
bk2 nMap maximals r p x =
  (++ maximals')
    . concatMap
      ( \v ->
          bk2
            nMap
            maximals'
            (S.insert v r)
            (S.intersection p (nMap M.! v))
            (S.intersection x (nMap M.! v))
      )
    . S.toList
    $ (p S.\\ largestNeighbourSet)
  where
    maximals' = if p == S.empty && x == S.empty then maximals ++ [r] else maximals
    -- choose node with the most neighbours as the pivot to minimise recursive calls
    largestNeighbourSet = maximumOn S.size . map (nMap M.!) . S.toList $ S.union p x

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

mkNeighbourSetMap :: [(String, String)] -> M.Map String (S.Set String)
mkNeighbourSetMap = M.map S.fromList . mkNeighbourMap

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