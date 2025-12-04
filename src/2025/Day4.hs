module Day4 where

import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Utils.BenchMark (runBenchMark)

-- first grid problem!

type Coord = (Int, Int)

type Grid = M.Map Coord Char

data SolutionState = SolutionState {_grid :: Grid, totalRemoved :: Int}

-- part1 :: IO Int
part1 :: IO ()
part1 = do
  input <- getInput
  runBenchMark solvePart1 input

-- for part 2 we need to keep the coordinates of the ones we remove, and actually remove them
-- we can do the classic trick of iterating to a fixed point
-- this might take a little while though, might take a while to get ones that are like way in the middle of a big cluster...

solvePart1 :: Grid -> Int
solvePart1 g =
  length
    . filter ((< 4) . length . filter ('@' ==) . neighbours g)
    . M.keys
    . M.filter (== '@')
    $ g

neighbours :: Grid -> Coord -> [Char]
neighbours grid (x, y) =
  mapMaybe
    (`M.lookup` grid)
    [ (x - 1, y - 1),
      (x - 1, y),
      (x - 1, y + 1),
      (x, y - 1),
      (x, y + 1),
      (x + 1, y - 1),
      (x + 1, y),
      (x + 1, y + 1)
    ]

getInput :: IO Grid
getInput = parseInput . lines <$> readFile "./fixtures/input4.txt"

parseInput :: [String] -> Grid
parseInput grid = M.fromList rawGrid
  where
    rawGrid =
      [ ((i, j), char)
        | (j, row) <- zipWithIndex grid,
          (i, char) <- zipWithIndex row
      ]

    -- based on the function of the same name in scala
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]
