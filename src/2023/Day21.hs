module Day21 where

-- clearly if a tile can be reached in an even number of steps and that number is at most 64
-- --then the elf can reach it in exactly 64 steps also
-- therefore we shall compute the minimum number of steps it takes to reach each tile
-- with a sort of "fan out" algorithm, probably BFS officially I guess
-- drawing this out an unimpeded elf can reach 4 tiles in 1 step, 8 tiles in 2 steps, 12 tiles in 3 steps and 4*n tile in n steps
-- so this feels quite reasonable to compute

import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

type Coords = (Int, Int)

type StepMap = M.Map Coords Int

data Tile = Blocked | Free | Start deriving (Eq)

type Grid = M.Map Coords Tile

data State = State {stepMap :: StepMap, nodes :: S.Set Coords, stepCount :: Int} deriving (Show)

-- 3816
-- 0.08 secs
part1 :: IO (Maybe Int)
part1 = solve . getGrid <$> getLines "./fixtures/input21.txt"

solve :: Grid -> Maybe Int
solve grid =
  fmap ((M.size . M.filter even) . stepMap)
    . find ((== 64) . stepCount)
    . iterate (step grid)
    . initState
    . getStartingCoords
    $ grid

initState :: Coords -> State
initState coords = State (M.singleton coords 0) (S.singleton coords) 0

step :: Grid -> State -> State
step g (State stepMap nodes stepCount) =
  let stepCount' = stepCount + 1
      allNeighbours = getAllNeighbours g nodes
      nodes' = S.difference allNeighbours (M.keysSet stepMap)
      newDistances = M.fromSet (const stepCount') nodes'
      stepMap' = M.union newDistances stepMap
   in State stepMap' nodes' stepCount'

getAllNeighbours :: Grid -> S.Set Coords -> S.Set Coords
getAllNeighbours g = foldl (\ns -> S.union ns . getNeighbours g) S.empty

getNeighbours :: Grid -> Coords -> S.Set Coords
getNeighbours g (x, y) = S.fromList . filter ((== Just Free) . (`M.lookup` g)) $ candidates
  where
    candidates = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

getGrid :: [String] -> Grid
getGrid inp = M.fromList [((x, y), toTile c) | (y, xs) <- zip [0 ..] inp, (x, c) <- zip [0 ..] xs]

toTile :: Char -> Tile
toTile 'S' = Start
toTile '#' = Blocked
toTile _ = Free

getStartingCoords :: Grid -> Coords
getStartingCoords = fst . head . M.toList . M.filter (== Start)

getLines :: FilePath -> IO [String]
getLines filePath = fmap lines (readFile filePath)