module Day6 where

import Data.List (find)
import Data.List.Extra (nubOrdOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S

type Coord = (Int, Int)

data Square = Obstacle | Free deriving (Show, Eq, Ord)

type Grid = M.Map Coord Square

data Direction = U | D | L | R deriving (Show, Eq, Ord)

data GuardState = GS
  { _direction :: Direction,
    _position :: Coord
  }
  deriving (Show, Eq, Ord)

-- I mean I probs could just use a list but whatevs
data GuardState' = GS'
  { _previousStates :: S.Set GuardState,
    _current :: GuardState,
    _isLoop :: Bool
  }
  deriving (Show, Eq, Ord)

{-
plan for part 1

- parse grid
- define some sort of guard state with direction and position
- write a function to move guard a single step forward if no obstacle
- write a function to turn guard to the right if there is an obstacle
- iterate these functions until you go out of bounds
- de-dupe all the squares you've visited + count them up!

 -}

{-
plan for part 2

I'm thinking maybe if I consider putting obstacles in the way for every single position
that would be in the way for the original guard's path that would tell me the answer.

The guard visited 5199 squares in part 1
This was from 6026 unique guard states
part 1 takes like 0.05 secs
so if I had to run that 6000 times we'd be looking at 5 minutes :(
that's assuming that the time for part 1 is average as well...
plus there'd need to be some cycle detection stuff which would make it take even longer

so I'm thinking there must be a cleverer way to work out where to put obstacles...

nonetheless it might be worth showing that this approach really would work on the toy example
might guide me to thinking of a better way...

 -}

{- hmm i can make my state smaller I think...-}

part1 :: IO Int
part1 = do
  input <- getInput
  let (grid, startPosition) = toGridWithStartPosition input
  let initGuardState = GS U startPosition
  return
    . S.size
    . S.fromList
    . map _position
    . walkGuard grid
    $ initGuardState

part2 :: IO Int
part2 = do
  input <- getInput
  let (grid, startPosition) = toGridWithStartPosition input
  let initGuardState = GS U startPosition

  return
    . length
    . filter (\(x, y, _) -> isLoop y x)
    . nubOrdOn (\(_, _, z) -> z) -- need this deduplication since we only want to consider placing each possible obstacle once (the first time it appears)
    . map (getNewGridAndStartingPosition (grid, startPosition))
    . walkGuard grid
    $ initGuardState

-- I've done this so that we're not starting from scratch every time!
-- can just use where we got up to in the initial part 1 traversal
getNewGridAndStartingPosition :: (Grid, Coord) -> GuardState -> (GuardState', Grid, Coord)
getNewGridAndStartingPosition (g, startingPos) gs = (GS' S.empty gs False, addObstacleToGrid (g, startingPos) obstPos, obstPos)
  where
    obstPos = _position . advance $ gs

walkGuard :: Grid -> GuardState -> [GuardState]
walkGuard grid = catMaybes . takeWhile isJust . iterate (wrap (step grid)) . Just

addObstacleToGrid :: (Grid, Coord) -> Coord -> Grid
addObstacleToGrid (g, startingPos) c = if startingPos == c then g else M.insert c Obstacle g

-- hmmmmmm actually we can't add an obstacle if it would get in the way of the old path now can we!!
-- welllllll actually we can but there wouldn't be any point since such an obstacle would have been added before anyway
-- so let's just dedupe a list of possible obstacle I guess
getAllValidObstaclePositions :: [GuardState] -> [Coord]
getAllValidObstaclePositions = S.toList . S.fromList . map (_position . advance)

isLoop :: Grid -> GuardState' -> Bool
isLoop grid = isJust . find _isLoop . walkGuardPart2 grid
  where
    -- the result of `walkGuardPart2` will either be an infinite list or will stop when the guard goes out of bounds
    -- just need a stopping condition to know we've got a cycle!
    walkGuardPart2 :: Grid -> GuardState' -> [GuardState']
    walkGuardPart2 grid' = catMaybes . takeWhile isJust . iterate (wrap (stepForPart2 grid')) . Just

-- only check for loops when we hit an obstacle and only update the set of visited states when we hit an obstacle
-- this made part 2 about 8x faster overall but it's still slow
stepForPart2 :: Grid -> GuardState' -> Maybe GuardState'
stepForPart2 grid (GS' prev curr _) = case nextSquare grid curr of
  Nothing -> Nothing
  Just Obstacle -> Just (GS' (S.insert curr prev) (turnRight curr) (S.member curr prev))
  Just Free -> Just (GS' prev (advance curr) False)

step :: Grid -> GuardState -> Maybe GuardState
step grid gs = fmap (\s -> step' s gs) . nextSquare grid $ gs
  where
    step' :: Square -> GuardState -> GuardState
    step' Obstacle = turnRight
    step' Free = advance

advance :: GuardState -> GuardState
advance (GS U (x, y)) = GS U (x, y - 1)
advance (GS D (x, y)) = GS D (x, y + 1)
advance (GS L (x, y)) = GS L (x - 1, y)
advance (GS R (x, y)) = GS R (x + 1, y)

turnRight :: GuardState -> GuardState
turnRight (GS U c) = GS R c
turnRight (GS D c) = GS L c
turnRight (GS L c) = GS U c
turnRight (GS R c) = GS D c

nextSquare :: Grid -> GuardState -> Maybe Square
nextSquare grid (GS U (x, y)) = M.lookup (x, y - 1) grid
nextSquare grid (GS D (x, y)) = M.lookup (x, y + 1) grid
nextSquare grid (GS L (x, y)) = M.lookup (x - 1, y) grid
nextSquare grid (GS R (x, y)) = M.lookup (x + 1, y) grid

toGridWithStartPosition :: [String] -> (Grid, Coord)
toGridWithStartPosition grid = (M.map toSquare . M.fromList $ rawGrid, startPosition)
  where
    startPosition = fst . head . filter ((== '^') . snd) $ rawGrid
    rawGrid =
      [ ((i, j), char)
        | (j, row) <- zipWithIndex grid,
          (i, char) <- zipWithIndex row
      ]

    -- based on the function of the same name in scala
    zipWithIndex :: [a] -> [(Int, a)]
    zipWithIndex = zip [0 ..]

    toSquare :: Char -> Square
    toSquare '#' = Obstacle
    toSquare _ = Free

-- surely something like this is in the standard lib...
wrap :: (a -> Maybe a) -> (Maybe a -> Maybe a)
wrap f = g
  where
    g Nothing = Nothing
    g (Just x) = f x

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input6.txt"