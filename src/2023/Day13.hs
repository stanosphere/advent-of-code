module Day13 where

import Data.Foldable (Foldable (toList))
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Sequence qualified as S (Seq, adjust', fromList)
import Data.Set qualified as ST (Set, difference, fromList, map, union)

type Pattern = [[Char]]

data Reflections = Reflections {vertical :: ST.Set Int, horizontal :: ST.Set Int} deriving (Show, Eq)

-- 42974
-- 0.02 secs
part1 :: IO Int
part1 = do
  patterns <- getPatterns "./fixtures/input13.txt"
  return . scoreReflections . map findOriginalRefl $ patterns

-- print . length . concat . concat $ patterns
-- gives me 16,394 so I reckon part 2 is brute forcible without much fuss

-- 27587
-- 0.19 secs
part2 :: IO Int
part2 = do
  patterns <- getPatterns "./fixtures/input13.txt"
  return . scoreReflections . map findNewRefl $ patterns

scoreReflections :: [Reflections] -> Int
scoreReflections = foldl (\count (Reflections v h) -> count + (sum . ST.map (* 1) $ v) + (sum . ST.map (* 100) $ h)) 0

findNewRefl :: Pattern -> Reflections
findNewRefl p =
  head
    . filter (\(Reflections v h) -> not . null $ ST.union v h)
    . map (diffWithOldRefl originalRefls . findOriginalRefl)
    . getAllReplacements
    $ p
  where
    originalRefls = findOriginalRefl p

diffWithOldRefl :: Reflections -> Reflections -> Reflections
diffWithOldRefl (Reflections oldV oldH) (Reflections newV newH) = Reflections (ST.difference newV oldV) (ST.difference newH oldH)

getAllReplacements :: Pattern -> [Pattern]
getAllReplacements ps = map ((map toList . toList) . (`applyUpdate` asSeq)) points
  where
    points = [(x, y) | (y, xs) <- zip [0 ..] ps, (x, _) <- zip [0 ..] xs]
    asSeq = S.fromList . map S.fromList $ ps
    applyUpdate :: (Int, Int) -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char)
    applyUpdate (x, y) = S.adjust' (S.adjust' swapChar x) y
    swapChar :: Char -> Char
    swapChar '#' = '.'
    swapChar '.' = '#'
    swapChar _ = undefined

findOriginalRefl :: Eq a => [[a]] -> Reflections
findOriginalRefl xs = Reflections vertical horizontal
  where
    vertical = ST.fromList . getAllReflections . transpose $ xs
    horizontal = ST.fromList . getAllReflections $ xs

getAllReflections :: Eq a => [a] -> [Int]
getAllReflections = map fst . filter (uncurry areReflections . snd) . getAllSplits
  where
    areReflections :: Eq a => [a] -> [a] -> Bool
    areReflections left right = all (uncurry (==)) (zip (reverse left) right)
    getAllSplits :: [a] -> [(Int, ([a], [a]))]
    getAllSplits xs = map (\i -> (i, splitAt i xs)) [1 .. length xs - 1]

getPatterns :: FilePath -> IO [[String]]
getPatterns filePath = splitOn [[]] . lines <$> readFile filePath
