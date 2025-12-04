module Day4 where

import Data.List (unfoldr)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Utils.BenchMark (runBenchMark)

-- first grid problem!

type Coord = (Int, Int)

type Grid = M.Map Coord Char

-- I'm fairly certain we don't need both _grid and _remaining
-- I think we only really need _remaining...
data SolutionState = SolutionState
  { _remaining :: S.Set Coord,
    _grid :: Grid,
    _totalRemoved :: Int
  }

part2 :: IO ()
part2 = do
  input <- getInput
  runBenchMark solvePart2 input

solvePart2 :: Grid -> Int
solvePart2 = last . unfoldr step . initialState

initialState :: Grid -> SolutionState
initialState g = SolutionState (M.keysSet g) g 0

step :: SolutionState -> Maybe (Int, SolutionState)
step (SolutionState remaining grid totalRemoved) =
  if S.null toRemove
    then Nothing
    else Just (totalRemoved', SolutionState remaining' grid' totalRemoved')
  where
    toRemove = S.filter (canBeRemoved grid) remaining
    totalRemoved' = totalRemoved + S.size toRemove
    remaining' = S.difference remaining toRemove
    grid' = M.withoutKeys grid toRemove

part1 :: IO ()
part1 = do
  input <- getInput
  runBenchMark solvePart1 input

solvePart1 :: Grid -> Int
solvePart1 g =
  length
    . filter (canBeRemoved g)
    . M.keys
    . M.filter (== '@')
    $ g

canBeRemoved :: Grid -> Coord -> Bool
canBeRemoved g = (< 4) . length . filter ('@' ==) . neighbours g

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

-- we only care about knowing where the paper rolls are
parseInput :: [String] -> Grid
parseInput grid = M.filter (== '@') . M.fromList $ rawGrid
  where
    rawGrid =
      [ ((i, j), char)
        | (j, row) <- zipWithIndex grid,
          (i, char) <- zipWithIndex row
      ]

    -- based on the function of the same name in scala
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]
