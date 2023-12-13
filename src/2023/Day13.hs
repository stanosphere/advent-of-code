module Day13 where

import Data.Foldable
import Data.List (transpose)
import Data.List.Split (chunksOf, splitOn)
import Data.Sequence qualified as S (Seq, adjust', fromList, update)

type Pattern = [[Char]]

data Reflections = Reflections {vertical :: [Int], horizontal :: [Int]} deriving (Show, Eq)

-- print . length . concat . concat $ patterns
-- give me 16,394 so I reckon that's brute forcible

showPattern :: Pattern -> IO ()
showPattern xs = putStrLn "" *> traverse_ putStrLn xs

part2 = do
  patterns <- getPatterns "./fixtures/input13Toy.txt"
  let firstPattern = ["##", ".."]
  let repacememtPatterns = getAllReplacements firstPattern
  let res = map findNewRefl patterns
  traverse_ print res

findNewRefl :: Pattern -> [Reflections]
findNewRefl p =
  filter (/= originalRefls)
    . filter (\(Reflections v h) -> not . null $ (v ++ h))
    . map getBothReflections
    . getAllReplacements
    $ p
  where
    originalRefls = getBothReflections p

swapChar :: Char -> Char
swapChar '#' = '.'
swapChar '.' = '#'
swapChar _ = undefined

getAllReplacements :: Pattern -> [Pattern]
getAllReplacements ps =
  let asSeq = S.fromList . map S.fromList $ ps
      updates = [(x, y) | (y, xs) <- zip [0 ..] ps, (x, _) <- zip [0 ..] xs]
      res = map (`applyUpdate` asSeq) $ updates
      res' = map (map toList . toList) res
   in res'

applyUpdate :: (Int, Int) -> S.Seq (S.Seq Char) -> S.Seq (S.Seq Char)
applyUpdate (x, y) = S.adjust' (S.adjust' swapChar x) y

-- 42974
-- 0.02 secs
part1 :: IO Int
part1 = do
  patterns <- getPatterns "./fixtures/input13.txt"
  return
    . foldl (\count (Reflections v h) -> count + (sum . map (* 1) $ v) + (sum . map (* 100) $ h)) 0
    . map getBothReflections
    $ patterns

getBothReflections :: Eq a => [[a]] -> Reflections
getBothReflections xs = Reflections vertical horizontal
  where
    vertical = getAllReflections . transpose $ xs
    horizontal = getAllReflections xs

getAllReflections :: Eq a => [a] -> [Int]
getAllReflections = map fst . filter (uncurry areReflections . snd) . getAllSplits
  where
    areReflections :: Eq a => [a] -> [a] -> Bool
    areReflections left right = all (uncurry (==)) (zip (reverse left) right)
    getAllSplits :: [a] -> [(Int, ([a], [a]))]
    getAllSplits xs = map (\i -> (i, splitAt i xs)) [1 .. length xs - 1]

getPatterns :: FilePath -> IO [[String]]
getPatterns filePath = splitOn [[]] . lines <$> readFile filePath
