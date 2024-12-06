module Day6 where

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

part1 :: IO Int
part1 = do
  input <- getInput
  let (grid, startPosition) = toGridWithStartPosition input
  let initGuardState = GS U startPosition
  let res = S.size . S.fromList . map _position . walkGuard grid $ initGuardState
  print (M.size grid)
  return res

walkGuard :: Grid -> GuardState -> [GuardState]
walkGuard grid = catMaybes . takeWhile isJust . iterate (wrap (step grid)) . Just

-- surely something like this is in the standard lib...
wrap :: (a -> Maybe a) -> (Maybe a -> Maybe a)
wrap f = g
  where
    g Nothing = Nothing
    g (Just x) = f x

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

getInput :: IO [String]
getInput = lines <$> readFile "./fixtures/input6.txt"