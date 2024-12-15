{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day14 where

import Control.Applicative (ZipList (ZipList, getZipList))
import Control.Arrow ((&&&))
import qualified Data.Bifunctor as BiFunc
import Data.List
  ( group,
    sort,
    tails,
  )
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Monoid (Sum (Sum, getSum), (<>))

type PairInsertionMap = M.Map String String

type PairFrequencies = M.Map String Integer

type CharFrequencies = M.Map String Integer

part1 :: Int
part1 =
  let freqMap = freq (iterate (step realInputMap) realInputSeed !! 10)
      occurances = map snd freqMap
   in maximum occurances - minimum occurances

part2 =
  let (_, freqMap) = getLetterFrequencies realInputMap realInputSeed !! 40
      occurances = M.elems freqMap
   in maximum occurances - minimum occurances

getLetterFrequencies ::
  PairInsertionMap -> String -> [(PairFrequencies, CharFrequencies)]
getLetterFrequencies mp seed = iterate (step' mp) (initPairFreq, initCharFreqs)
  where
    initCharFreqs =
      M.fromList . map (BiFunc.bimap (: []) fromIntegral) . freq $ seed
    initPairFreq =
      M.fromList . map (BiFunc.second fromIntegral) . freq . windows 2 $ seed

-- for part2 I want to keep track of both the frequency of pairs and the frequency of individual characters
step' ::
  PairInsertionMap ->
  (PairFrequencies, CharFrequencies) ->
  (PairFrequencies, CharFrequencies)
step' mp (pfs, cfs) =
  (updatePairFrequencies mp pfs, updateCharFrequencies mp (pfs, cfs))

updateCharFrequencies ::
  PairInsertionMap -> (PairFrequencies, CharFrequencies) -> CharFrequencies
updateCharFrequencies mp (pfs, cfs) =
  foldr (uncurry (M.insertWith (+)) . BiFunc.first (mp M.!)) cfs
    . M.toList
    $ pfs

updatePairFrequencies :: PairInsertionMap -> PairFrequencies -> PairFrequencies
updatePairFrequencies mp = mkMapInteger' . concatMap getNextPairs . M.toList
  where
    getNextPairs (k, v) =
      let [k1, k2] = getNextPairOfPairs mp k in [(k1, v), (k2, v)]

-- does not compile
-- mkMapInteger :: (Ord k) => [(k, Integer )] -> M.Map k Integer
-- mkMapInteger = mkMap

mkMapInteger' :: (Ord k) => [(k, Integer)] -> M.Map k Integer
mkMapInteger' = M.map getSum . mkMap . map (BiFunc.second Sum)

mkMap :: (Ord k, Monoid v) => [(k, v)] -> M.Map k v
mkMap = foldr (uncurry (M.insertWith (<>))) M.empty

getNextPairOfPairs :: PairInsertionMap -> String -> [String]
getNextPairOfPairs mp pair =
  let middle = mp M.! pair
   in case pair of
        [l, r] -> [l : middle, middle ++ [r]]

experiment = map freq (iterate (step toyInputMap) toyInputSeed)

experiment2 = map length (iterate (step realInputMap) realInputSeed)

-- part 1 stuff here
step :: PairInsertionMap -> String -> String
step mp s = head s : (concatMap (processPair mp) . windows 2 $ s)

processPair :: PairInsertionMap -> String -> String
processPair mp pair = case M.lookup pair mp of
  Nothing -> pair
  Just x -> x ++ [pair !! 1]

freq :: (Ord a) => [a] -> [(a, Int)]
freq = map (head &&& length) . group . sort

-- input data below here
toyInputSeed = "NNCB"

-- 4 * 4 = 16 entries
toyInputMap :: PairInsertionMap
toyInputMap =
  M.fromList
    . map ((\[x, y] -> (x, y)) . splitOn " -> ")
    $ [ "CH -> B",
        "HH -> N",
        "CB -> H",
        "NH -> C",
        "HB -> C",
        "HC -> B",
        "HN -> C",
        "NN -> C",
        "BH -> H",
        "NC -> B",
        "NB -> B",
        "BN -> B",
        "BB -> N",
        "BC -> B",
        "CC -> N",
        "CN -> C"
      ]

realInputSeed = "BNSOSBBKPCSCPKPOPNNK"

-- 10 * 10 = 100 entries
realInputMap :: PairInsertionMap
realInputMap =
  M.fromList
    . map ((\[x, y] -> (x, y)) . splitOn " -> ")
    $ [ "HH -> N",
        "CO -> F",
        "BC -> O",
        "HN -> V",
        "SV -> S",
        "FS -> F",
        "CV -> F",
        "KN -> F",
        "OP -> H",
        "VN -> P",
        "PF -> P",
        "HP -> H",
        "FK -> K",
        "BS -> F",
        "FP -> H",
        "FN -> V",
        "VV -> O",
        "PS -> S",
        "SK -> N",
        "FF -> K",
        "PK -> V",
        "OF -> N",
        "VP -> K",
        "KB -> H",
        "OV -> B",
        "CH -> F",
        "SF -> F",
        "NH -> O",
        "NC -> N",
        "SP -> N",
        "NN -> F",
        "OK -> S",
        "BB -> S",
        "NK -> S",
        "FH -> P",
        "FC -> S",
        "OB -> P",
        "VS -> P",
        "BF -> S",
        "HC -> V",
        "CK -> O",
        "NP -> K",
        "KV -> S",
        "OS -> V",
        "CF -> V",
        "FB -> C",
        "HO -> S",
        "BV -> V",
        "KS -> C",
        "HB -> S",
        "SO -> N",
        "PH -> C",
        "PN -> F",
        "OC -> F",
        "KO -> F",
        "VF -> V",
        "CS -> O",
        "VK -> O",
        "FV -> N",
        "OO -> K",
        "NS -> S",
        "KK -> C",
        "FO -> S",
        "PV -> S",
        "CN -> O",
        "VC -> P",
        "SS -> C",
        "PO -> P",
        "BN -> N",
        "PB -> N",
        "PC -> H",
        "SH -> K",
        "BH -> F",
        "HK -> O",
        "VB -> P",
        "NV -> O",
        "NB -> C",
        "CP -> H",
        "NO -> K",
        "PP -> N",
        "CC -> S",
        "CB -> K",
        "VH -> H",
        "SC -> C",
        "KC -> N",
        "SB -> B",
        "BP -> P",
        "KP -> K",
        "SN -> H",
        "KF -> K",
        "KH -> B",
        "HV -> V",
        "HS -> K",
        "NF -> B",
        "ON -> H",
        "BO -> P",
        "VO -> K",
        "OH -> C",
        "HF -> O",
        "BK -> H"
      ]

-- random utility stuff here
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails
