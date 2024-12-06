module Day6 where

import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as S

type Coord = (Int, Int)

data Square = Obstacle | Free deriving (Show)

type Grid = M.Map Coord Square

data Direction = U | D | L | R deriving (Show)

data GuardState = GS
  { _direction :: Direction,
    _position :: Coord
  }
  deriving (Show)

{-
plan for part 1

- parse grid
- define some sort of guard state with direction and position
- write a function to move guard a single step forward if no obstacle
- write a function to turn guard to the right if there is an obstacle
- iterate these functions until you go out of bounds
- de-dupe all the squares you've visited + count them up!

 -}

part1 = do
  input <- getInput
  let (grid, startPosition) = toGridWithStartPosition input
  let initGuardState = GS U startPosition
  let res = S.size . S.fromList . map _position . walkGuard grid $ initGuardState
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