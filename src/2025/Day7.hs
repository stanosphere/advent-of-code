module Day7 where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Set as S

type BeamPositions = S.Set Int

type SplitterPositions = S.Set Int

part1 :: IO Int
part1 = do
  (initialPosition, splitters) <- getInput
  let res = foldl' step (initialPosition, 0) splitters
  return (snd res)

step :: (BeamPositions, Int) -> SplitterPositions -> (BeamPositions, Int)
step (beams, hits) splitters = (newBeams, hits + S.size splittersHit)
  where
    splittersHit = S.intersection beams splitters
    splitBeams = S.unions . S.map (\x -> S.insert (x + 1) . S.insert (x - 1) $ S.empty) $ splittersHit
    newBeams = S.union splitBeams . (`S.difference` splittersHit) $ beams

getInput :: IO (BeamPositions, [SplitterPositions])
getInput = do
  rawInput <- lines <$> readFile "./fixtures/input7.txt"
  let (h : t) = rawInput
  return (parseBeamRow h, map parseSplitterRow t)
  where
    parseSplitterRow = parseRow '^'
    parseBeamRow = parseRow 'S'
    parseRow c = S.fromList . map fst . filter ((== c) . snd) . zip [0 ..]