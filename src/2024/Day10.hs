module Day10 where

import Data.Char (digitToInt)
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

type Coord = (Int, Int)

type Grid = M.Map Coord Int

type Trail = [(Coord, Int)]

type TrailState = [Trail]

part1 :: IO Int
part1 = do
  (grid, startingPositions) <- getInput
  let startingState = map (\c -> [(c, 0)]) startingPositions
  return . length . nub . map (\xs -> (head xs, last xs)) . findTrails grid $ startingState

part2 :: IO Int
part2 = do
  (grid, startingPositions) <- getInput
  let startingState = map (\c -> [(c, 0)]) startingPositions
  return . sum . M.elems . frequencies . map (fst . last) . findTrails grid $ startingState

-- taken from Day 1 of this year, might be worth popping it in some util folder
frequencies :: (Ord a) => [a] -> M.Map a Int
frequencies = foldr incrementMap M.empty
  where
    incrementMap x = M.insertWith (+) x 1

findTrails :: Grid -> TrailState -> TrailState
findTrails g = last . take 10 . iterate (step g)

step :: Grid -> TrailState -> TrailState
step g = concatMap stepTrail
  where
    stepTrail :: Trail -> [Trail]
    stepTrail (x : xs) = map (\n -> n : x : xs) . neighbours g $ x
    stepTrail _ = undefined

neighbours :: Grid -> (Coord, Int) -> [(Coord, Int)]
neighbours grid ((x, y), value) =
  filter ((== (value + 1)) . snd)
    . mapMaybe (\c -> fmap (\res -> (c, res)) . M.lookup c $ grid)
    $ [ (x, y - 1),
        (x, y + 1),
        (x + 1, y),
        (x - 1, y)
      ]

getInput :: IO (Grid, [Coord])
getInput = parseInput . lines <$> readFile "./fixtures/input10.txt"

parseInput :: [String] -> (Grid, [Coord])
parseInput grid = (M.fromList rawGrid, startPositions)
  where
    startPositions = map fst . filter ((== 0) . snd) $ rawGrid
    rawGrid =
      [ ((i, j), parseSquare char)
        | (j, row) <- zipWithIndex grid,
          (i, char) <- zipWithIndex row
      ]

    parseSquare '.' = 999
    parseSquare c = digitToInt c

    -- based on the function of the same name in scala
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]
