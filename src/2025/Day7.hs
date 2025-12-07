module Day7 where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import qualified Data.Set as S

type BeamPositions = S.Set Int

type SplitterPositions = S.Set Int

type BeamPositions' = M.Map Int Int

part1 :: IO Int
part1 = do
  (initialPosition, splitters) <- getInput
  return . snd . foldl' step (initialPosition, 0) $ splitters

part2 :: IO Int
part2 = do
  (initialPosition, splitters) <- getInput'
  return . M.foldr (+) 0 . foldl' step' initialPosition $ splitters

step' :: BeamPositions' -> SplitterPositions -> BeamPositions'
step' beams spittersInRow = foldl' updateBeamPosition M.empty . M.keys $ beams
  where
    splittersHit = S.intersection (M.keysSet beams) spittersInRow

    updateBeamPosition :: BeamPositions' -> Int -> BeamPositions'
    updateBeamPosition acc c =
      M.insert
        c
        (contribFromBeam + contribFromSplitterL + contribFromSplitterR)
        acc
      where
        isSplitterAbove = S.member c splittersHit
        contribFromBeam = if isSplitterAbove then 0 else beams M.! c
        -- if there's a splitter to the left we get its contribution
        contribFromSplitterL = if S.member (c - 1) splittersHit then beams M.! (c - 1) else 0
        -- if there's a splitter to the right we get its contribution
        contribFromSplitterR = if S.member (c + 1) splittersHit then beams M.! (c + 1) else 0

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

getInput' :: IO (BeamPositions', [SplitterPositions])
getInput' = do
  rawInput <- lines <$> readFile "./fixtures/input7.txt"
  let (h : t) = rawInput
  return (parseFirstRow h, map parseSplitterRow t)
  where
    parseSplitterRow = S.fromList . map fst . filter ((== '^') . snd) . zip [0 ..]
    parseFirstRow = M.fromList . zipWith (\i c -> (if c == 'S' then (i, 1) else (i, 0))) [0 ..]